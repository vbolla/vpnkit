
type t = {
  upstream: Dns_forward_config.t;
  search: string list;
}
(** DNS configuration *)

val to_string: t -> string

val of_string: string -> t option

val of_resolv_conf: string -> t option
