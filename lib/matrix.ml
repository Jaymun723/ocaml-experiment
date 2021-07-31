(* let dims m = (Array.length m, Array.length m.(0))

   let create = Array.make_matrix

   let merge f = Array.map2 @@ fun a b -> Array.map2 f a b

   let scale m s = Array.map (fun n -> Array.map (fun x -> x *. s) n) m

   let add = merge ( +. )

   let ( + ) = add

   let sub = merge ( -. )

   let ( - ) = sub

   let hadamart_product = merge ( *. )

   let mult m0 m1 =
     let x0, y0 = dims m0 in
     let x1, y1 = dims m1 in
     if y0 <> x1 then failwith "incompatible matrices!"
     else
       let res_matrix = Array.make_matrix x0 y1 0. in
       for i = 0 to Stdlib.(x0 - 1) do
         for j = 0 to Stdlib.(y1 - 1) do
           for k = 0 to Stdlib.(y0 - 1) do
             res_matrix.(i).(j) <- res_matrix.(i).(j) +. (m0.(i).(k) *. m1.(k).(j))
           done
         done
       done;
       res_matrix

   let ( * ) = mult *)

let create x y generate_value =
  let sub_create i = List.init y @@ generate_value i in
  List.init x sub_create

let zeroes x y =
  let zero_fn _ _ = 0. in
  create x y zero_fn

let dims m = (List.length m, List.length @@ List.nth m 0)

let merge f =
  let sub_merge = List.map2 f in
  List.map2 sub_merge

let map f = List.map @@ List.map f

let scale s = map @@ fun x -> x *. s

let add = merge ( +. )

let ( + ) = add

let sub = merge ( -. )

let ( - ) = sub

let hadamart_product = merge ( *. )

let row = List.nth

let col m i = List.map (fun x -> List.nth x i) m

let mult m0 m1 =
  let x, _ = dims m0 and _, y = dims m1 in
  let template = create x y @@ fun _ _ -> 0 in
  let inner_product i j = Vector.dot (row m0 i) (col m1 j) in
  List.mapi (fun i l -> List.mapi (fun j _ -> inner_product i j) l) template

let ( * ) = mult
