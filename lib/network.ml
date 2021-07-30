let random_float =
  let _ = Random.self_init in
  fun () -> Random.float 1.

class network (sizes : int list) =
  object
    val num_layers = List.length sizes

    val mutable biases =
      List.map (fun l -> Vector.create l random_float) @@ List.tl sizes

    val mutable weights =
      let a = sizes |> List.rev |> List.tl |> List.rev in
      let b = List.tl sizes in
      let matrix_sizes = List.combine a b in
      List.map (fun (x, y) -> Matrix.create x y)

    method get_num_layers = num_layers

    method get_biases = biases
  end
