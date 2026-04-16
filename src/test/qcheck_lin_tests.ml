
open QCheck
open Lin
open Universal_instances



let int_small = QCheck.small_int


(* STACK *)

module StackSpec (S : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialStack.op -> int -> 'a option
end) = struct
  type t = int S.t

  let init () = S.create 4
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "push" (fun s x tid -> S.apply s (Sequential.SequentialStack.Push x) tid)
        (t @-> int_small @-> int @-> returning option int);

      val_ "pop" (fun s tid -> S.apply s Sequential.SequentialStack.Pop tid)
        (t @-> int @-> returning option int);
    ]
end

module LFStackTest = Lin_domain.Make(StackSpec(LFStack))
module WFStackTest = Lin_domain.Make(StackSpec(WFStack))




module QueueSpec (Q : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialQueue.op -> int -> 'a option
end) = struct
  type t = int Q.t

  let init () = Q.create 4
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "enq" (fun q x tid -> Q.apply q (Sequential.SequentialQueue.Enqueue x) tid)
        (t @-> int_small @-> int @-> returning option int);

      val_ "deq" (fun q tid -> Q.apply q Sequential.SequentialQueue.Dequeue tid)
        (t @-> int @-> returning option int);
    ]
end

module LFQueueTest = Lin_domain.Make(QueueSpec(LFQueue))
module WFQueueTest = Lin_domain.Make(QueueSpec(WFQueue))




