open Lib

let () =
  let m1 =
    Matrix.add (Array.make_matrix 2 2 1.) @@ Array.make_matrix 2 2 0.235
  in
  let m2 = Matrix.add m1 @@ Array.make_matrix 2 2 0.5 in
  Printers.pretty_print_matrix m2
