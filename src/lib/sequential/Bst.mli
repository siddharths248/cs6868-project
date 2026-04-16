type 'a tree = 
  Empty
  | Node of {l : 'a tree ; v : 'a ; r : 'a tree}

type 'a rnode = {
  compare : 'a -> 'a -> int;
  root : 'a tree;
}

val empty : ('a -> 'a -> int) -> 'a rnode

val to_list : 'a rnode -> 'a list

val insert : 'a rnode -> 'a -> 'a rnode

val remove : 'a rnode -> 'a -> 'a rnode

val is_present : 'a rnode -> 'a -> bool