let rec create length generate_value =
  match length with
  | 0 -> []
  | l -> generate_value () :: create (l - 1) generate_value

let add = List.map2 Stdlib.( +. )

let ( + ) = add

let sub = List.map2 Stdlib.( -. )

let ( - ) = sub

let dot = List.fold_left2 (fun acc x y -> (x *. y) +. acc) 0.

let ( * ) = dot
