open Src
open Sequential

module type SeqLike = sig
  type 'a state
  type 'a op
  val apply : 'a op -> 'a state -> 'a state * 'a option
  val empty : 'a state
end

module MakeLF (Seq : SeqLike) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) LFUniversal.t

  let create = LFUniversal.create

  let apply obj op tid =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply op state in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty, None) in
    let (_new_obj, result) = LFUniversal.apply obj initial_obj invoc tid in
    result
end

module MakeWF (Seq : SeqLike) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) WFUniversal.t

  let create = WFUniversal.create

  let apply obj op tid =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply op state in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty, None) in
    let (_new_obj, result) = WFUniversal.apply obj initial_obj invoc tid in
    result
end
