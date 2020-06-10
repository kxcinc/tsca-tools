open Tscalib

type Error_monad.error += Encoding_error of Data_encoding.Binary.write_error
type Error_monad.error +=
       Decoding_error of [ `Read_error of Data_encoding.Binary.read_error
                         | `Message of string]

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"MichelsonEncodingError"
    ~title:"Michelson Encoding Error"
    ~description:"An error has occurred while encoding Michelson value"
    ~pp:(fun ppf e -> Format.fprintf ppf "%a" Data_encoding.Binary.pp_write_error e)
    Data_encoding.Binary.write_error_encoding
    (function Encoding_error e -> Some e | _ -> None)
    (fun e -> Encoding_error e) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"MichelsonDecodingError"
    ~title:"Michelson Decoding Error"
    ~description:"An error has occurred while decoding Michelson value"
    ~pp:(fun ppf -> function
      | Decoding_error (`Read_error e) ->
         Format.fprintf ppf "%a" Data_encoding.Binary.pp_read_error e
      | Decoding_error (`Message str) -> Format.pp_print_string ppf str
      | _ -> Stdlib.failwith "panic" (* this should never happen *))
    Data_encoding.(
    union [case ~title:"Read_error" (Tag 0) Data_encoding.Binary.read_error_encoding
             (function Decoding_error (`Read_error e) -> Some e | _ -> None)
             (fun e -> Decoding_error (`Read_error e));
           case ~title:"Message" (Tag 1) Data_encoding.string
             (function Decoding_error (`Message s) -> Some s | _ -> None)
             (fun s -> Decoding_error (`Message s));])
    (function (Decoding_error _ as x) -> Some x | _ -> None)
    (fun x -> x)

include struct
  module Michelson_v1_emacs = ProtoClient.Michelson_v1_emacs
  module Michelson_v1_entrypoints = ProtoClient.Michelson_v1_entrypoints
  module Michelson_v1_error_reporter = ProtoClient.Michelson_v1_error_reporter
  module Michelson_v1_macros = ProtoClient.Michelson_v1_macros
  module Michelson_v1_parser = ProtoClient.Michelson_v1_parser
  module Michelson_v1_printer = ProtoClient.Michelson_v1_printer
  module Michelson_v1_primitives = Protocol.Michelson_v1_primitives

  module Script_expr_hash = Protocol.Script_expr_hash
  module Script_int_repr = Protocol.Script_int_repr
  module Script_interpreter = Protocol.Script_interpreter
  module Script_ir_annot = Protocol.Script_ir_annot
  module Script_ir_translator = Protocol.Script_ir_translator
  module Script_repr = Protocol.Script_repr
  module Script_tc_errors = Protocol.Script_tc_errors
  module Script_tc_errors_registration = Protocol.Script_tc_errors_registration
  module Script_timestamp_repr = Protocol.Script_timestamp_repr
  module Script_typed_ir = Protocol.Script_typed_ir

  type 'a lwt = 'a Lwt.t
  type 'a tzresult = 'a Tezos_error_monad.Error_monad.tzresult

  type prim = Michelson_v1_primitives.prim
  type 'a canonical = 'a Micheline.canonical
  type parsed_michelson = Michelson_v1_parser.parsed = {
    source : string;
    unexpanded : string canonical;
    expanded : Alpha_context.Script.expr;
    expansion_table : (int * (Micheline_parser.location * int list)) list;
    unexpansion_table : (int * int) list;
  }

  type ex_ty = Script_ir_translator.ex_ty
end

open struct
  let to_tzresult f = function
      Error e -> error (f e)
    | Ok x -> ok x

  let packed_data_prefix = Bytes.of_string "\005"

  exception Deprefix_exception
  let deprefix prefix bytes =
    let len = Bytes.length bytes in
    let plen = Bytes.length prefix in
    let err = Decoding_error (`Message "prefix check failed") in
    fail_when (len < plen) err
    >>=? fun () ->
    try Bytes.iteri (fun i c ->
            if Bytes.get bytes i <> c then raise Deprefix_exception)
          prefix;
        return (Bytes.sub bytes plen (len-plen))
    with Deprefix_exception -> fail err
end

let michelson_encoding : prim canonical Data_encoding.t =
  Micheline.canonical_encoding_v1
    ~variant:"michelson_v1"
    Michelson_v1_primitives.prim_encoding

let parse_expr ?check str : parsed_michelson tzresult lwt =
  Lwt.return @@ Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_expression ?check str

let parse_script ?check str : parsed_michelson tzresult lwt =
  Lwt.return @@ Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_toplevel ?check str

let unparse_expr = Format.asprintf "%a" Michelson_v1_printer.print_expr

let unparse_toplevel ?type_map script =
  let unparsed = Michelson_v1_printer.unparse_toplevel ?type_map script
  in unparsed.source

let prims_of_strings = Michelson_v1_primitives.prims_of_strings
let prim_of_string = Michelson_v1_primitives.prim_of_string
let strings_of_prims = Michelson_v1_primitives.strings_of_prims
let string_of_prim = Michelson_v1_primitives.string_of_prim

open struct
  let root_node : parsed_michelson -> (int, prim) Micheline.node tzresult =
    fun { unexpanded; _ } ->
    let open Environment.Error_monad in
    prims_of_strings unexpanded >|? Micheline.root
    |> Environment.wrap_error

  let ir_parse_packable_type ctxt typ : (ex_ty * Alpha_context.t) tzresult =
    root_node typ >>? fun typ ->
    Script_ir_translator.parse_packable_ty ctxt ~legacy:true typ
    |> Environment.wrap_error
end

let pack_data_untyped data : bytes tzresult lwt =
  Lwt.return &
  data.unexpanded
  |> Michelson_v1_primitives.prims_of_strings
  |> Environment.wrap_error
  >>? fun converted ->
  Data_encoding.Binary.to_bytes michelson_encoding converted
  |> to_tzresult (fun e -> Encoding_error e)
  >>? fun bytes ->
  Bytes.cat packed_data_prefix bytes |> ok

let pack_data_typed ~ctxt ~typ data : bytes tzresult lwt =
  let open Script_ir_translator in
  root_node typ >>?= fun typ ->
  root_node data >>?= fun node ->
  let legacy = true in
  parse_packable_ty ctxt ~legacy typ |> Environment.wrap_error
  >>?= fun (Ex_ty typ, ctxt) ->
  parse_data ctxt ~legacy typ node >|= Environment.wrap_error
  >>=? fun (data, ctxt) ->
  pack_data ctxt typ data >|= Environment.wrap_error
  >>=? fun (bytes, _ctxt) -> return bytes

let pack_data ?typ : parsed_michelson -> bytes tzresult lwt =
  fun data ->
  match typ with
  | None -> pack_data_untyped data
  | Some typ ->
     fake_alpha_context() >>=? fun ctxt ->
     pack_data_typed ~ctxt ~typ data

let unpack_data_untyped data : prim canonical tzresult lwt =
  deprefix packed_data_prefix data
  >>=? fun data ->
  Data_encoding.Binary.of_bytes michelson_encoding data
  |> to_tzresult (fun e -> Decoding_error (`Read_error e))
  |> Lwt.return

let unpack_data_typed ~ctxt ~typ data : prim canonical tzresult lwt =
  unpack_data_untyped data
  >>=? fun node ->
  ir_parse_packable_type ctxt typ |> Lwt.return
  >>=? fun (Ex_ty typ, ctxt) -> begin
    let open Script_ir_translator in
    let open Environment.Error_monad in
    parse_data ctxt ~legacy:false typ (Micheline.root node)
    >>=? fun (x, ctxt) ->
    unparse_data ctxt Readable typ x
    >>=? fun (node, _ctxt) ->
    Micheline.strip_locations node |> return
    end
  >|= Environment.wrap_error

let unpack_data ?typ : bytes -> prim canonical tzresult lwt =
  fun data ->
  match typ with
  | None -> unpack_data_untyped data
  | Some typ ->
     fake_alpha_context() >>=? fun ctxt ->
     unpack_data_typed ~ctxt ~typ data

let reformat_script : string -> string tzresult lwt =
  fun script ->
  parse_script ~check:false script >>=? fun parsed ->
  (Michelson_v1_printer.unparse_toplevel parsed.expanded).source |> return

let typecheck_script : parsed_michelson -> Script_tc_errors.type_map tzresult lwt =
  fun script ->
  fake_alpha_context() >>=? fun ctxt ->
  Script_ir_translator.typecheck_code ctxt script.expanded >|= Environment.wrap_error
  >>= fun res ->
  (res >>? fun (res, _ctxt) -> ok res)
  |> Lwt.return

module Streamlined = struct
  exception Tezos_errors of trace

  open struct
    let strip_tzresult = function
      | Ok x -> x
      | Error trace -> raise (Tezos_errors trace)
  end

  let pack_data_err ?check:(check=false)
        ?tztyp tzdata : bytes tzresult =
    Lwt_main.run &
    parse_expr ~check tzdata
    >>=? fun data ->
    match tztyp with
    | None -> pack_data_untyped data
    | Some tztyp ->
       parse_expr ~check tztyp
       >>=? fun typ ->
       fake_alpha_context() >>=? fun ctxt ->
       pack_data_typed ~ctxt ~typ data

  let pack_data_opt ?check ?tztyp tzdata : bytes option =
    pack_data_err ?check ?tztyp tzdata
    |> Result.to_option

  let pack_data ?check ?tztyp tzdata : bytes =
    pack_data_err ?check ?tztyp tzdata
    |> strip_tzresult

  let unpack_data_err ?check:(check=false)
        ?tztyp tzpdata : string tzresult =
    Lwt_main.run &
    begin match tztyp with
    | None -> unpack_data_untyped tzpdata
    | Some tztyp ->
       parse_expr ~check tztyp
       >>=? fun typ ->
       fake_alpha_context() >>=? fun ctxt ->
       unpack_data_typed ~ctxt ~typ tzpdata
    end
    >>=? fun data ->
    Format.asprintf "%a" Michelson_v1_printer.print_expr data |> return

  let unpack_data_opt ?check ?tztyp tzpdata : string option =
    unpack_data_err ?check ?tztyp tzpdata
    |> Result.to_option

  let unpack_data ?check ?tztyp tzpdata : string =
    unpack_data_err ?check ?tztyp tzpdata
    |> strip_tzresult

  let reformat_script_err script : string tzresult =
    reformat_script script |> Lwt_main.run
  let reformat_script_opt script : string option =
    reformat_script_err script |> Result.to_option
  let reformat_script script : string =
    reformat_script_err script |> strip_tzresult

  let typecheck_script_err script : Script_tc_errors.type_map tzresult =
    typecheck_script script |> Lwt_main.run
  let typecheck_script_opt script : Script_tc_errors.type_map option =
    typecheck_script_err script |> Result.to_option
end
