(** Redis protocol *)

(** Redis protocol messages *)
type t =
  | Simple_string of string (** Simple (binary-unsafe) strings *)
  | Error of string
  | Integer of string
  | Bulk_string of string option (** Bulk (binary-safe) string if Some and null string if None *)
  | Array of t array option (** Array if Some and null array if None *)

(** Return a string representation of the protocol message (as OCaml source) *)
val to_string : t -> string

(** Return a human-readable string representation of the protocol message, much like the one produced by [redis-cli] *)
val to_string_hum : t -> string

(** Redis Serialization Protocol (RESP) encoder/decoder. Encodes/decodes Redis protocol messages to/from RESP byte strings.
  @see <http://redis.io/topics/protocol> Redis Protocol Specification *)
module Resp : sig

  (** Return the encoding of the message as a byte string. *)
  val encode : t -> string option

  (** Return the encoding of the message as a byte string. *)
  val encode_exn : t -> string

  (** Append the encoding of the message to the given buffer. *)
  val encode_to_buffer_exn : t -> Buffer.t -> unit

  (** Try to decode the given byte string. *)
  val decode : string -> t option

  (** Try to decode the given byte string (throws [Invalid_encoding] and [Trailing_garbage].  *)
  val decode_exn : string -> t

  (** The message could not be encoded because a simple string contains a CR or LF character. *)
  exception Simple_string_contains_CR_or_LF

  (** No message could not be decoded due to a protocol error. *)
  exception Invalid_encoding

  (** A message was decoded, but the end of the bytestring was not reached. Contains the decoded message and the offset at which the "garbage" starts. *)
  exception Trailing_garbage of t*int

end

(** Builds protocol messages for redis commands.
  @see <http://redis.io/topics/protocol#sending-commands-to-a-redis-server> Redis Protocol Specification: Sending commands to a Redis Server *)
module Redis_command : sig

  (** Build a command with an arbitrary number of arguments, e.g.
    {[build "SET" ["fooKey";"barValue"]]} *)
  val build : command:string -> string list -> t

  (** Build a command with no arguments, e.g.
    {[build0 "SHUTDOWN"]} *)
  val build0 : string -> t

  (** Build a command with 1 argument, e.g.
    {[build1 "INCR" "intKey"]} *)
  val build1 : command:string -> string -> t

  (** Build a command with 2 arguments *)
  val build2 : command:string -> string -> string -> t

  (** Build a command with 3 arguments *)
  val build3 : command:string -> string -> string -> string -> t

  (** Build a command with 4 arguments *)
  val build4 : command:string -> string -> string -> string -> string -> t

  (** Build a command with 5 arguments *)
  val build5 : command:string -> string -> string -> string -> string -> string -> t

  (** Build a command with 6 arguments *)
  val build6 : command:string -> string -> string -> string -> string -> string -> string -> t

end
