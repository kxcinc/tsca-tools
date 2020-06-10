type known_protocols =
  | TzProtoPs9mPmXa
  | TzProtoPtCJ7pwo
  | TzProtoPsYLVpVv
  | TzProtoPsddFKi3
  | TzProtoPt24m4xi
  | TzProtoPsBABY5H
  | TzProtoPsBabyM1
  | TzProtoPsCARTHA

let tscalib_protocol = TzProtoPsCARTHA
(** the current protocol of Tscalib *)

module TzPervasives = Tezos_base.TzPervasives
module ProtoClient = Tezos_client_006_PsCARTHA

include Tezos_client_base
include Tezos_client_base_unix
include Tezos_micheline

include TzPervasives

module Protocol = Tezos_protocol_006_PsCARTHA.Protocol
module Environment = Protocol.Environment
module Alpha_context = Protocol.Alpha_context

module Log = Internal_event.Legacy_logging.Make (struct
                 let name = "tscalib"
               end)

open struct
  let _fake_alpha_context =
    lazy (
        Lwt_main.run &
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
          Alpha_context.prepare
            ~level ~predecessor_timestamp ~timestamp ~fitness
            context
          >|= Environment.wrap_error
      )
end

let fake_alpha_context () : Alpha_context.t tzresult Lwt.t =
  Lazy.force _fake_alpha_context |> Lwt.return
