(** Lock-free stack interface using LFUniversal *)

(** The type of the lock-free stack *)
type 'a t

(** [create num_threads] creates a new lock-free stack for [num_threads] threads. *)
val create : int -> 'a t

(** [apply stack op tid] applies the operation [op] to the lock-free stack [stack]
    on behalf of thread [tid].
    - [op]: The operation to apply (e.g., Push or Pop).
    - [tid]: The thread ID of the thread applying the operation.
    Returns the result of the operation. *)
val apply : 'a t -> 'a SequentialStack.op -> int -> 'a option
