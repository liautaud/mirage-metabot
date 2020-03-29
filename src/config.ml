open Mirage

(** {1 Setting up keys for the unikernel.}

    The list of available configuration keys can be obtained by running
    [mirage help configure]. These keys can either be set at configuration time,
    by passing an argument or environment variable to [mirage configure], or at
    runtime when the unikernel is compiled to a native target. *)
let port =
  let doc =
    Key.Arg.info ~doc:"TCP port used by the HTTP server." ~env:"METABOT_PORT"
      [ "port" ]
  in
  Key.(create "port" Arg.(opt int 8080 doc))

let client_id =
  let doc =
    Key.Arg.info ~doc:"Client ID for the Slack API." ~env:"METABOT_CLIENT_ID"
      [ "client_id" ]
  in
  Key.(create "client-id" Arg.(required string doc))

let client_secret =
  let doc =
    Key.Arg.info ~doc:"Client secret for the Slack API."
      ~env:"METABOT_CLIENT_SECRET" [ "client_secret" ]
  in
  Key.(create "client-secret" Arg.(required string doc))

let signing_secret =
  let doc =
    Key.Arg.info ~doc:"Signing secret for the Slack API."
      ~env:"METABOT_SIGNING_SECRET" [ "signing_secret" ]
  in
  Key.(create "signing-secret" Arg.(required string doc))

let bot_token =
  let doc =
    Key.Arg.info ~doc:"Bot user access token for the Slack API."
      ~env:"METABOT_BOT_TOKEN" [ "bot_token" ]
  in
  Key.(create "bot-token" Arg.(required string doc))

(** {2 Configuring the unikernel.} *)
let main =
  let packages =
    [
      (* package "tls-mirage"; *)
      package "conduit-mirage";
      package "cohttp-mirage";
      package "yojson";
      package "digestif";
      package "uuidm";
      package "ppx_deriving";
    ]
  in
  let k = Key.abstract in
  let keys =
    [ k port; k client_id; k client_secret; k signing_secret; k bot_token ]
  in

  foreign ~packages ~keys "Bot.Main"
  @@ pclock @-> conduit @-> resolver @-> http @-> job

(** {3 Instanciating and registering the unikernel.} *)
let () =
  let stack = generic_stackv4 default_network in
  let conduit = conduit_direct ~tls:false stack in
  let resolver = resolver_dns stack in
  let server = cohttp_server conduit in

  register "metabot"
    [ main $ default_posix_clock $ conduit $ resolver $ server ]
