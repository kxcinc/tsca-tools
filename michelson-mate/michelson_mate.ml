let (&) = (@@)

module Log = Internal_event.Legacy_logging.Make (struct
  let name = "michelson-mate.main"
end)

module Micheline = Tezos_micheline.Micheline
module Protocol = Tezos_protocol_006_PsCARTHA.Protocol

let parse_expr expr =
  let open Tezos_micheline in
  let open Tezos_client_006_PsCARTHA in
  Lwt.return @@ Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_expression expr
let data_parameter = Clic.parameter (fun _ data -> parse_expr data)

let bytes_parameter =
  let bytes_of_prefixed_string s =
    try
      if String.length s < 2 || s.[0] <> '0' || s.[1] <> 'x' then raise Exit
      else return (Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2))))
    with _ ->
      failwith "Invalid bytes, expecting hexadecimal notation (e.g. 0x1234abcd)"
  in
  Clic.param
    (Clic.parameter (fun _ s -> bytes_of_prefixed_string s))

let type_arg =
  Clic.arg
    ~long:"type"
    ~short:'T'
    ~doc:"expected type of the data"
    ~placeholder:"type"
    data_parameter

let michelson_expr_encoding =
  Tezos_micheline.Micheline.canonical_encoding_v1
    ~variant:"michelson_v1"
    Protocol.Michelson_v1_primitives.prim_encoding

type Error_monad.error += Encoding_error of Data_encoding.Binary.write_error
type Error_monad.error +=
   | Decoding_error of Data_encoding.Binary.read_error
   | Decoding_error_str of string

let packed_data_prefix = Bytes.of_string "\005"

let pack_data_untyped data =
  Lwt.return &
  Tezos_client_006_PsCARTHA.Michelson_v1_parser.(data.unexpanded)
  |> Protocol.Michelson_v1_primitives.prims_of_strings
  |> Protocol.Environment.wrap_error
  >>? fun converted ->
  Data_encoding.Binary.to_bytes michelson_expr_encoding converted
  |> begin function
       | Error e -> error (Encoding_error e)
       | Ok bytes -> ok bytes
     end
  >>? fun bytes ->
  Bytes.cat packed_data_prefix bytes |> ok

let pack_data_typed ~ctxt data typ =
  Lwt.map Protocol.Environment.wrap_error &
  let open Protocol.Script_ir_translator in
  let open Protocol.Environment.Error_monad in
  let legacy = true in
  let tr data = Tezos_client_006_PsCARTHA.Michelson_v1_parser.(data.unexpanded) in
  let (typ, node) = tr typ, tr data in
  Protocol.Michelson_v1_primitives.prims_of_strings node |> Lwt.return
  >>=? fun node ->
  Protocol.Michelson_v1_primitives.prims_of_strings typ |> Lwt.return
  >>=? fun typ ->
  let (typ, node) = (Micheline.root typ, Micheline.root node) in
  parse_packable_ty ctxt ~legacy typ |> Lwt.return
  >>=? fun (Ex_ty typ, ctxt) ->
  parse_data ctxt ~legacy typ node
  >>=? fun (data, ctxt) ->
  Protocol.Script_ir_translator.pack_data ctxt typ data
  >>=? fun (bytes, _ctxt) -> return bytes

let fake_alpha_context =
  let open Tezos_006_PsCARTHA_test_helpers in
  let module Protocol = Tezos_protocol_006_PsCARTHA.Protocol in
  Context.init 1
  >>=? fun (block, _accounts) ->
  let Block.{
      context;
      header =
        { shell =
            {level; timestamp = predecessor_timestamp;
             fitness; _};
          _ }
      ; _ } = block in
  let now = Unix.time () |> Int64.of_float in
  let timestamp = Time.Protocol.of_seconds now in
  (Protocol.Alpha_context.prepare
     ~level ~predecessor_timestamp ~timestamp ~fitness
     context)
  >|= Protocol.Environment.wrap_error

let pack_data ?typ data =
  match typ with
  | None -> pack_data_untyped data
  | Some typ ->
     fake_alpha_context >>=? fun ctxt ->
     pack_data_typed ~ctxt data typ

let deprefix prefix bytes =
  let len = Bytes.length bytes in
  let plen = Bytes.length prefix in
  let err = Decoding_error_str "prefix check failed" in
  fail_when (len < plen) err
  >>=? fun () ->
  try prefix
      |> Bytes.iteri (fun i c ->
             if Bytes.get bytes i <> c then raise Not_found);
      return (Bytes.sub bytes plen (len-plen))
  with _ -> fail err

let unpack_data_untyped data =
  deprefix packed_data_prefix data
  >>=? fun data ->
  Data_encoding.Binary.of_bytes michelson_expr_encoding data
  |> begin function
       | Error e -> fail (Decoding_error e)
       | Ok decoded -> return decoded
     end

let unpack_data_typed ~ctxt data typ =
  unpack_data_untyped data
  >>=? fun node -> begin
      let open Protocol.Script_ir_translator in
      let open Protocol.Environment.Error_monad in
      let legacy = true in
      Tezos_client_006_PsCARTHA.Michelson_v1_parser.(typ.unexpanded)
      |>  Protocol.Michelson_v1_primitives.prims_of_strings |> Lwt.return
      >>=? fun typ ->
      parse_packable_ty ctxt ~legacy (Micheline.root typ) |> Lwt.return
      >>=? fun (Ex_ty typ, ctxt) ->
      parse_data ctxt ~legacy typ
        (Micheline.root node)
      >>=? fun (x, ctxt) ->
      unparse_data ctxt Readable typ x
       >>=? fun (node, _ctxt) ->
       Micheline.strip_locations node |> return
    end
  >|= Protocol.Environment.wrap_error

let unpack_data ?typ (data : bytes) =
  match typ with
  | None -> unpack_data_untyped data
  | Some typ ->
     fake_alpha_context >>=? fun ctxt ->
     unpack_data_typed ~ctxt data typ

let group =
  { Clic.name = "michelson-mate";
    title = "Utility commands for the Michelson language"; }

let slurp file =
  let open Lwt_io in
  let chop = fun () -> match file with
    | "-" -> Lwt.return stdin
    | _ -> open_file ~mode:input file
  in Lwt.try_bind chop
    (fun ch -> read ch >>= return)
    (fun exn -> Error_monad.fail (Exn exn))

let commands =
  let open Clic in
  let greeting =
    command
      ~group
      ~desc:"Greet the user"
      no_options
      (fixed ["greetings"])
      (fun () (cctx : Client_context.full) ->
        cctx#message "Greetings from Michelson Mate"
        >>= return)
  in
  let reformat =
    command ~group ~desc:"reformat a Michelson contract"
      no_options
      (prefix "reformat"
       @@ (string ~name:"path|-" ~desc:"path to the contract file")
       @@ stop)
      (fun () tzfile (cctx : Client_context.full) ->
        slurp tzfile
        >>=? fun contents ->
        let open Tezos_client_006_PsCARTHA in
        let (parsed, errors) = Michelson_v1_parser.parse_toplevel
              ~check:false contents in
        unless (errors = []) (fun () -> cctx#error "%a" pp_print_error errors)
        >>=? fun () ->
        let unparsed = Michelson_v1_printer.unparse_toplevel parsed.expanded
        in cctx#answer "%s" unparsed.source
        >>= return
      )
  in
  let pack =
    command ~group ~desc:"pack a Michelson value"
      (args1 type_arg)
      (prefix "pack"
       @@ (param ~name:"data" ~desc:"Michelson value to be packed" data_parameter)
       @@ stop)
      (fun (typ) data (cctx : Client_context.full) ->
        pack_data ?typ data
        >>=? fun packed ->
        cctx#answer "0x%a" Hex.pp (Hex.of_bytes packed)
        >>= return
      )
  in
  let unpack =
    command ~group ~desc:"pack a Michelson value"
      (args1 type_arg)
      (prefix "unpack"
       @@ (bytes_parameter
             ~name:"data"
             ~desc:"Michelson value to be unpacked, \
                    in Hex with '0x' prefix")
       @@ stop)
      (fun (typ) data (cctx : Client_context.full) ->
        let open Tezos_client_006_PsCARTHA in
        unpack_data ?typ data
        >>=? fun unpacked ->
        let unparsed = Michelson_v1_printer.unparse_expression unpacked in
        cctx#answer "%s" unparsed.source
        >>= return
      )
  in
  let typecheck_script =
    command ~group ~desc:"pack a Michelson value"
      no_options
      (prefixes ["typecheck"; "script"]
       @@ (string ~name:"path|-" ~desc:"path to the contract file")
       @@ stop)
      (fun () tzfile (cctx : Client_context.full) ->
        slurp tzfile
        >>=? fun contents ->
        let open Tezos_client_006_PsCARTHA in
        let (parsed, errors) = Michelson_v1_parser.parse_toplevel
              ~check:true contents in
        unless (errors = []) (fun () -> cctx#error "%a" pp_print_error errors)
        >>=? fun () ->
        fake_alpha_context >>=? fun ctxt ->
        Protocol.Script_ir_translator.typecheck_code ctxt parsed.expanded
        >|= Protocol.Environment.wrap_error
        >>= fun res ->
        let res = res >>? fun (res, ctxt) ->
                  (ok (res, Protocol.Alpha_context.Gas.level ctxt))
        in
        Client_proto_programs.print_typecheck_result
          ~emacs:false ~show_types:false ~print_source_on_error:false
          parsed res
          cctx
      )
  in
  [greeting; reformat; pack; unpack; typecheck_script]

let select_commands _ctxt _cli_args =
  commands
  @ Client_keys_commands.commands (Some `Sandbox)
  |> return

let () =
  Client_main_run.run
    ~log:(Log.fatal_error "%s")
    (module Client_config)
    ~select_commands
