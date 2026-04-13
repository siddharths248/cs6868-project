(** Wait-free stack implementation using WFUniversal *)

type 'a t = {
  wfu : ('a SequentialStack.state * 'a option) WFUniversal.t;
}

let create num_threads =
  {
    wfu = WFUniversal.create num_threads;
  }

let apply stack op tid =
  let invoc (state, _result) =
    SequentialStack.apply state op
  in
  let initial_obj = ([], None) in
  let (_new_state, result) = WFUniversal.apply stack.wfu invoc initial_obj tid in
  result