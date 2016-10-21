
type t = {
  upstream: Dns_forward.Config.t;
  search: string list;
}
(** DNS configuration *)

val compare: t -> t -> int
(** The usual comparison operator over configurations *)

(** {1 Serialisation and deserialisation}

    These functions convert to and from human-readable strings. These strings
    are used in the database keys.
*)
val to_string: t -> string
(** Serialise configuration to a human-readable string *)

val of_string: string -> t option
(** Deserialise configuration from a human-readable string *)

val of_resolv_conf: string -> t option
(** Parse a Unix-style /etc/resolv.conf and return the configuration. Note a
    Unix /etc/resolv.conf cannot express all possible configurations. *)
