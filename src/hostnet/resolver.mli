
type t = {
  upstream: Dns_forward_config.t;
  search: string list;
}

val to_string: t -> string

val parse_resolvers: string -> t option
(** [parse_resolvers data] parses DNS resolvers stored in a string *)
