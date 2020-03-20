open Mirage

(** {1 Setting up keys for the unikernel.}

    The list of available configuration keys can be obtained by running
    [mirage help configure]. These keys can either be set at configuration time,
    by passing an argument or environment variable to [mirage configure], or at
    runtime when the unikernel is compiled to a native target. *)
let port =
  let doc =
    Key.Arg.info ~doc:"The TCP port to use for the HTTP server." [ "port" ]
  in
  Key.(create "port" Arg.(opt int 8080 doc))

(** {2 Configuring the unikernel.} *)
let main =
  let packages =
    [
      (* package "tls-mirage"; *)
      package "conduit-mirage";
      package "cohttp-mirage";
    ]
  in
  let keys = List.map Key.abstract [ port ] in

  foreign ~packages ~keys "Bot.Main"
  @@ stackv4 @-> conduit @-> resolver @-> http @-> job

(** {3 Instanciating and registering the unikernel.} *)
let () =
  let stack = generic_stackv4 default_network in
  let conduit = conduit_direct ~tls:false stack in
  let resolver = resolver_dns stack in
  let server = cohttp_server conduit in

  register "metabot" [ main $ stack $ conduit $ resolver $ server ]
