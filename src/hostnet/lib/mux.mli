module Make(Netif: V1_LWT.NETWORK) : sig
  include V1_LWT.NETWORK

  (** A simple ethernet multiplexer/demultiplexer

     An instance has an underlying ethernet NETWORK which it receives
     packets from. If a packet matches a rule associated with a downstream
     port, it is sent there. Packets which don't match any rules are sent
     to the default callback for processing. All transmissions from downstream
     ports are sent on the underlying network.

     The default callback (set by [listen]) acts as the control-path: it's function
     is to accept flows and set up new rules.

  *)

  val connect: Netif.t -> t Lwt.t
  (** Connect a multiplexer/demultiplexer and return a [t] which behaves like
      a V1_LWT.NETWORK representing the multiplexed end. *)

  type rule = Ipaddr.V4.t
  (** We currently support matching on IPv4 destination addresses only *)

  val port: t -> rule -> (module V1_LWT.NETWORK) Lwt.t
  (** Given a rule, create a network which will receive traffic matching the
      rule. *)

end
