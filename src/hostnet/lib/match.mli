
type t
(** A predicate which is true if we should record this ethernet frame *)

val all: t
(** Match all frames *)

val ethernet: t -> t
(** Matches all ethernet frames *)

val (or): t -> t -> t
(** [a or b] matches [a] or [b] *)

val ipv4:
   ?src:Ipaddr.V4.t
   -> ?dst:Ipaddr.V4.t
   -> unit -> t -> t
(** Matches all IP frames *)

val tcp:
   ?src:int
-> ?dst:int
-> unit -> t -> t
(** Matches all TCP frames *)

val udp:
   ?src:int
-> ?dst:int
-> unit -> t -> t
(** Matches all UDP frames *)

val bufs: t -> Cstruct.t list -> bool
