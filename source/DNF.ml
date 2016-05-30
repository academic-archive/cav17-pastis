(* Quentin Carbonneaux - 2016 *)

type 'a t = 'a list list

let lift x = [[x]]

let true_ = [[]]
let false_ = []
let is_true a = List.mem [] a
let is_false a = a = []

let disjunct = List.rev_append

let rec conjunct a b =
  match a with
  | [] -> []
  | x :: a ->
    disjunct (List.map ((@) x) b) (conjunct a b)
