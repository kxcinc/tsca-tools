open Tezos_client_006_PsCARTHA

type parsed = Michelson_v1_parser.parsed = {
  source : string;
  unexpanded : string Tezos_micheline.Micheline.canonical;
  expanded : Tezos_raw_protocol_006_PsCARTHA.Alpha_context.Script.expr;
  expansion_table :
    (int * (Tezos_micheline.Micheline_parser.location * int list)) list;
  unexpansion_table : (int * int) list;
}

let parse = Michelson_v1_parser.parse_toplevel ~check:false
let unparse ({ expanded; _ } : parsed) =
  let unparsed = Michelson_v1_printer.unparse_toplevel expanded in
  unparsed.source

let load_from_channel ch =
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch; str

let emit_to_channel ch str =
  output_string ch str; flush ch

let () =
  let (parsed, errors) = load_from_channel stdin |> parse in
  if errors <> []
  then begin
      Michelson_v1_error_reporter.report_errors
        ~details:true ~show_source:true
        Format.err_formatter errors;
      exit 2;
    end;
  parsed |> unparse |> emit_to_channel stdout;
  print_newline(); flush_all()
