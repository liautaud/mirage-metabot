include module type of Slack_types

module Make (Clock : PCLOCK) (Server : SERVER) : sig
  type t

  val create :
    ctx:Client.ctx ->
    bot_token:string ->
    client_id:string ->
    client_secret:string ->
    signing_secret:string ->
    unit ->
    t
  (** Creates an API handler. *)

  val respond_authorize :
    t -> Server.conn -> Request.t -> string -> (Response.t * Body.t) Lwt.t
  (** Responds to an authorization request. *)

  val respond_events :
    t -> Server.conn -> Request.t -> string -> (Response.t * Body.t) Lwt.t
  (** Responds to an event notification request. *)

  val register : t -> event_type -> event_handler -> uuid
  (** Registers a handler for a given event type. *)

  val unregister : t -> uuid -> unit
  (** Unregisters the event handler with the given uuid. *)
end
