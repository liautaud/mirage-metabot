open Lwt.Infix

module type STACKV4 = Mirage_stack.V4

module type CONDUIT = Conduit_mirage.S

module type RESOLVER = Resolver_lwt.S

module type SERVER = Cohttp_lwt.S.Server

module Main
    (Stack : STACKV4)
    (Conduit : CONDUIT)
    (Resolver : RESOLVER)
    (Server : SERVER) =
struct
  (* Aliases for frequently used modules. *)
  module Client = Cohttp_mirage.Client
  module Request = Cohttp.Request
  module Response = Cohttp.Response
  module Body = Cohttp_lwt.Body

  (** Replies to a given HTTP request. *)
  let respond_http conn request body = Server.respond ()

  (** Starts the HTTP server with a given callback. *)
  let start_http server callback =
    server (`TCP (Key_gen.port ())) (Server.make ~callback ())

  (** Entrypoint for the unikernel. *)
  let start stack conduit resolver server =
    Lwt.join [ start_server server respond_http ]
end
