open Lwt.Infix

module type PCLOCK = Mirage_clock.PCLOCK

module type CONDUIT = Conduit_mirage.S

module type RESOLVER = Resolver_lwt.S

module type SERVER = Cohttp_lwt.S.Server

(* Aliases for frequently used modules. *)
module Client = Cohttp_mirage.Client
module Request = Cohttp.Request
module Body = Cohttp_lwt.Body

module Main
    (Clock : PCLOCK)
    (Conduit : CONDUIT)
    (Resolver : RESOLVER)
    (Server : SERVER) =
struct
  module Api = Slack.Make (Clock) (Server)

  (** Attempt to retrieve the public IP of the unikernel. *)
  let get_ip ctx =
    let open Yojson.Basic.Util in
    let uri = Uri.of_string "http://api.ipify.org?format=json" in
    Client.get ~ctx uri >>= fun (_, body) ->
    Body.to_string body >|= fun body ->
    Yojson.Basic.from_string body |> member "ip" |> to_string

  (** Print the startup message to the logs. *)
  let motd ctx =
    get_ip ctx >|= fun ip ->
    let port = Key_gen.port () in
    let base = Format.sprintf "http://%s:%d" ip port in
    Logs.info (fun f -> f "Listening for HTTP requests on 0.0.0.0:%d" port);
    Logs.info (fun f ->
        f "Your IP is %s, so you should be able to access these URLs:" ip;
        f " * %s/authorize  [Adds the bot to your Slack workspace]" base;
        f " * %s/events     [Handles incoming Slack events]" base)

  (** Starts the HTTP server. *)
  let start_http routes server =
    let port = Key_gen.port () in
    let callback conn req body =
      Body.to_string body >>= fun body ->
      let resource = Request.resource req in
      Logs.debug (fun f ->
          f "Received a request on %s\nRequest: %a\nBody: %s" resource
            Request.pp_hum req body);
      match List.assoc_opt resource routes with
      | Some c -> c conn req body
      | None -> Server.respond_not_found ()
    in
    server (`TCP port) (Server.make ~callback ())

  (** Routes for the HTTP server. *)
  let routes api =
    [
      ("/authorize", Api.respond_authorize api);
      ("/events", Api.respond_events api);
    ]

  (** Entrypoint for the unikernel. *)
  let start _clock conduit resolver server =
    let ctx = Client.ctx resolver conduit in
    let bot_token = Key_gen.bot_token () in
    let client_id = Key_gen.client_id () in
    let client_secret = Key_gen.client_secret () in
    let signing_secret = Key_gen.signing_secret () in

    let api =
      Api.create ~ctx ~bot_token ~client_id ~client_secret ~signing_secret ()
    in
    let routes = routes api in
    let _ =
      Api.register api `Message
        (fun ~app_id:_
             ~team_id:_
             ~event_type:_
             ~event_id:_
             ~event_time:_
             event_body
             ->
          let open Yojson.Basic.Util in
          let channel = event_body |> member "channel" |> to_string in
          let text = event_body |> member "text" |> to_string in
          Api.post_message api ~channel ~text ())
    in
    motd ctx >>= fun () -> start_http routes server
end
