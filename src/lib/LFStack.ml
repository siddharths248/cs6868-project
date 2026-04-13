(** Lock-free stack implementation using LFUniversal *)

(** The type of the lock-free stack *)
type 'a t = {
  lfu : ('a SequentialStack.state * 'a option) LFUniversal.t;
  num_threads : int;
}

(** [create num_threads] creates a new lock-free stack for [num_threads] threads. *)
let create num_threads = {
    lfu = LFUniversal.create num_threads;
    num_threads;
  }

(** [apply stack op tid] applies the operation [op] to the lock-free stack [stack]
    on behalf of thread [tid].
    - [op]: The operation to apply (e.g., Push or Pop).
    - [tid]: The thread ID of the thread applying the operation.
    Returns the result of the operation. *)
let apply stack op tid =
  let invoc (state, result) =  SequentialStack.apply state op in
  let initial_obj = ([], None) in 
  let (_new_state, result) = LFUniversal.apply stack.lfu invoc initial_obj tid in
  result
