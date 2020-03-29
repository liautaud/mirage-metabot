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

  val post_message :
    t ->
    channel:string ->
    text:string ->
    ?attachments:Yojson.Basic.t ->
    ?blocks:Yojson.Basic.t ->
    ?link_names:bool ->
    ?markdown:bool ->
    ?reply_broadcast:bool ->
    ?thread_ts:string ->
    ?username:string ->
    unit ->
    unit Lwt.t
  (** Sends a message to a Slack workspace.

      - [channel]: Channel, private group, or IM channel to send message to.
      - [text]: Full text of the message (or fallback when used with [blocks]).
      - [attachments]: JSON-based array of structured attachments.
      - [blocks]: JSON-based array of structured blocks.
      - [link_names]: Whether to find and link channel names and usernames.
      - [markdown]: Whether the Markdown tags in [text] will be parsed.
      - [reply_broadcast]: When [thread_ts] is used, indicates whether reply
        should be made visible to everyone in the channel or conversation.
      - [reply_ts]: Used in threads only, [ts] of the parent message.
      - [username]: A custom user name displayed along the message. *)
end
