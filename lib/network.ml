let random_float =
  let _ = Random.self_init in
  fun (_ : int) (_ : int) -> Random.float 1.

class network (sizes : int list) =
  object (self)
    val num_layers = List.length sizes

    val mutable biases =
      List.map (fun l -> Matrix.create l 1 random_float) @@ List.tl sizes

    val mutable weights =
      let a = List.tl sizes in
      let b = sizes |> List.rev |> List.tl |> List.rev in
      let matrix_sizes = List.combine a b in
      List.map (fun (x, y) -> Matrix.create x y random_float) matrix_sizes

    val mutable eta = 0.01

    method get_eta = eta

    method set_eta new_eta = eta <- new_eta

    method get_num_layers = num_layers

    method get_biases = biases

    method get_weights = weights

    method feed_forward (input : float list list) =
      let fold_fn input (w, b) = Matrix.((w * input) + b) in
      List.fold_left fold_fn input @@ List.combine weights biases

    method backprop (x : float list list) (y : float list list) = ([ x ], [ y ])

    method update_mini_batch (batch : (float list list * float list list) list)
        =
      let open Matrix in
      let l = float_of_int @@ List.length batch in
      let zeroer m =
        let x, y = dims m in
        zeroes x y
      in
      let nabla_w = List.map zeroer weights in
      let nabla_b = List.map zeroer biases in
      let combine = List.map2 ( + ) in
      let fold_fn (w, b) (x, y) =
        let dw, db = self#backprop x y in
        (combine w dw, combine b db)
      in
      let nabla_w, nabla_b = List.fold_left fold_fn (nabla_w, nabla_b) batch in
      let update x dx = x - scale (eta /. l) dx in
      weights <- List.map2 update weights nabla_w;
      biases <- List.map2 update biases nabla_b
  end

let sigmoid z = 1.0 /. (1. +. exp (-.z))

let n = new network [ 3; 2; 1 ]

(* let feed_forward (input : float list list) =
   let aux index =
     let w = List.nth n#get_weights index in
     let b = List.nth n#get_biases index in
     let product = Matrix.(mult w input + b) in
     Matrix.map sigmoid product
   in
   aux 0 *)

let backprop (x : float list list) (y : float list list) = ([ x ], [ y ])

let update_mini_batch batch =
  let open Matrix in
  let zeroer m =
    let x, y = dims m in
    zeroes x y
  in
  let nabla_w = List.map zeroer n#get_weights in
  let nabla_b = List.map zeroer n#get_biases in
  let start = (nabla_w, nabla_b) in
  let combine = List.map2 ( + ) in
  let fold_fn (w, b) (x, y) =
    let dw, db = backprop x y in
    (combine w dw, combine b db)
  in
  List.fold_left fold_fn start batch
