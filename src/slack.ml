open Lwt.Infix
include Slack_types

let slack_base = Uri.of_string "https://slack.com/"

module Make (Clock : PCLOCK) (Server : SERVER) = struct
  type t = {
    ctx : Client.ctx;
    bot_token : string;
    client_id : string;
    client_secret : string;
    signing_secret : string;
    handlers : (uuid, event_handler) Hashtbl.t;
    handlers_by_type : (event_type, uuid) Hashtbl.t;
  }

  let create ~ctx ~bot_token ~client_id ~client_secret ~signing_secret () =
    let handlers = Hashtbl.create 255 in
    let handlers_by_type = Hashtbl.create 255 in
    {
      ctx;
      bot_token;
      client_id;
      client_secret;
      signing_secret;
      handlers;
      handlers_by_type;
    }

  (* Returns the current POSIX time in seconds. *)
  let now () =
    let d, ps = Clock.now_d_ps () in
    let s1 = Int64.mul (Int64.of_int d) 86_400L in
    let s2 = Int64.div ps 1_000_000_000_000L in
    Int64.add s1 s2 |> Int64.to_int

  (* Raises Invalid_signature if the request is not signed properly.
     See https://api.slack.com/docs/verifying-requests-from-slack. *)
  let verify_signature t req body =
    try
      (* Check the provided timestamp. *)
      let h = Request.headers req in
      let ts = Header.get h "x-slack-request-timestamp" |> Option.get in
      let ts = int_of_string ts in
      if abs (now () - ts) > 300 then raise Invalid_signature;

      (* Check the provided signature. *)
      let (module Sha256) = Digestif.module_of Digestif.sha256 in
      let ss = Header.get h "x-slack-signature" |> Option.get in
      if not (String.equal (String.sub ss 0 3) "v0=") then
        raise Invalid_signature;
      let digest = String.sub ss 3 (String.length ss - 3) |> Sha256.of_hex in
      let key = t.signing_secret in
      let content = Printf.sprintf "v0:%d:%s" ts body in
      let target = Sha256.hmac_string ~key content in
      if not (Sha256.equal digest target) then raise Invalid_signature;

      ()
    with _ -> raise Invalid_signature

  let respond_authorize t _conn req body =
    Server.respond_string ~status:`OK ~body:"authorize" ()

  let to_event_type = function
    | "message" -> `Message
    | "app_mention" -> `App_Mention
    | s -> failwith (Printf.sprintf "Unsupported event type %s." s)

  let respond_events t _conn req body =
    verify_signature t req body;

    (* Parse the JSON payload. *)
    let json = Yojson.Basic.from_string body in
    let open Yojson.Basic.Util in
    let challenge = json |> member "challenge" |> to_string_option in
    match challenge with
    | Some c ->
        (* Respond to the Slack API challenge if needed. *)
        Logs.info (fun f -> f "Received Slack challenge %s" c);
        Server.respond_string ~status:`OK ~body:c ()
    | None ->
        (* Dispatch the event to all the event handlers. *)
        let app_id = json |> member "api_app_id" |> to_string in
        let team_id = json |> member "team_id" |> to_string in
        let event_id = json |> member "event_id" |> to_string in
        let event_time = json |> member "event_time" |> to_int in
        let event_body = json |> member "event" in
        let event_type =
          json |> member "event" |> member "type" |> to_string |> to_event_type
        in
        Logs.info (fun f ->
            f "Received Slack event %s (of type %a)" event_id pp_event_type
              event_type);

        Hashtbl.find_all t.handlers_by_type event_type
        |> List.map (Hashtbl.find t.handlers)
        |> List.map (fun h ->
               (* Ignore failures from registered handlers. *)
               Lwt.catch
                 (fun () ->
                   h ~app_id ~team_id ~event_type ~event_id ~event_time
                     event_body)
                 (fun e ->
                   Logs.err (fun f ->
                       f "Handler failed: %s" (Printexc.to_string e));
                   Lwt.return_unit))
        |> Lwt.join
        >>= fun () -> Server.respond_string ~status:`OK ~body:"" ()

  let register t ty handler =
    let uuid = Uuidm.v `V4 in
    Hashtbl.add t.handlers uuid handler;
    Hashtbl.add t.handlers_by_type ty uuid;
    uuid

  let unregister t uuid =
    Hashtbl.filter_map_inplace
      (fun _ v -> if Uuidm.equal v uuid then None else Some v)
      t.handlers_by_type;
    Hashtbl.remove t.handlers uuid

  (* Posts JSON data to the Slack API. *)
  let post_json t path data =
    let open Yojson.Basic.Util in
    let flatten =
      List.filter_map (fun (x, y) -> Option.map (fun y -> (x, y)) y)
    in
    let headers = Header.init_with "content-type" "application-json" in
    let uri = Uri.with_path slack_base path in
    let data = `Assoc (("token", `String t.bot_token) :: flatten data) in
    let body = Yojson.Basic.to_string data in
    Logs.info (fun f -> f "Sending API request to %a" Uri.pp uri);
    Logs.debug (fun f -> f "Body: %s" body);
    Client.post ~ctx:t.ctx ~body:(Body.of_string body) ~headers uri
    >>= fun (res, body) ->
    (* Check the response status. *)
    Body.to_string body >>= fun body ->
    Logs.debug (fun f -> f "Response: %a" Response.pp_hum res);
    Logs.debug (fun f -> f "Response body: %s" body);
    let payload = Yojson.Basic.from_string body in
    if payload |> member "ok" |> to_bool then Lwt.return_unit
    else
      let error = payload |> member "error" |> to_string in
      Lwt.fail_with (Printf.sprintf "Slack API failed: %s" error)

  let post_message t ~channel ~text ?attachments ?blocks ?(link_names = true)
      ?(markdown = true) ?(reply_broadcast = false) ?thread_ts ?username () =
    let to_json_string s = `String s in
    post_json t "api/chat.postMessage"
      [
        ("channel", Some (`String channel));
        ("text", Some (`String text));
        ("attachments", attachments);
        ("blocks", blocks);
        ("link_names", Some (`Bool link_names));
        ("markdown", Some (`Bool markdown));
        ("reply_brodcast", Some (`Bool reply_broadcast));
        ("thread_ts", Option.map to_json_string thread_ts);
        ("username", Option.map to_json_string username);
      ]
end
