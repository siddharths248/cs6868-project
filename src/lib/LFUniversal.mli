

type 'a t

(** [create num_threads] creates a new universal construction instance for num_threads threads. *)
val create : int -> 'a t

(** [apply lfu_obj invoc new_obj tid] is called by the thread with id [tid] and 
    applies the operation represented by [invoc] to the object [new_obj] in the universal construction instance [lfu_obj] *)
val apply : 'a t -> ('a -> 'a) -> 'a -> int -> 'a