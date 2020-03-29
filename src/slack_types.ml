module type PCLOCK = Mirage_clock.PCLOCK

module type SERVER = Cohttp_lwt.S.Server

module Client = Cohttp_mirage.Client
module Request = Cohttp.Request
module Header = Cohttp.Header
module Body = Cohttp_lwt.Body
module Response = Cohttp.Response

exception Invalid_signature

type json = Yojson.Basic.t
(** JSON-formatted data. *)

type uuid = Uuidm.t
(** Unique identifier of a registered event handler. *)

type event_type = [ `Message | `App_Mention ] [@@deriving show]
(** Slack event type. *)

type event_body = json
(** Slack event body. *)

type event_handler =
  app_id:string ->
  team_id:string ->
  event_type:event_type ->
  event_id:string ->
  event_time:int ->
  event_body ->
  unit Lwt.t
(** Handler for Slack events. *)
