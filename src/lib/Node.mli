
type 'a t

(** Creates a new node with the given value and sequence number *)
val create : ('a -> 'a) -> int -> 'a t

(** Returns the node with the maximum sequence number in the array *)
val max : 'a t Array.t -> 'a t

(** Updates the sequence number of the given node *)
val set_seq : 'a t -> int -> unit

val get_seq : 'a t -> int

(** Returns the CAS consensus instance associated with the given node *)
val get_decide_next : 'a t -> 'a t CASConsensus.t

val set_next : 'a t -> 'a t -> unit

val get_next : 'a t -> 'a t option

val get_invoc : 'a t -> ('a -> 'a)