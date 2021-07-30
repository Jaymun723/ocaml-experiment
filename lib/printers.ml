let print_array f = function
  | [||] -> ()
  | [| a |] -> f a
  | arr ->
      let n = Array.length arr in
      print_string "[";
      f arr.(0);
      for i = 1 to n - 1 do
        print_string " ";
        f arr.(i)
      done;
      print_string "]"

let print_matrix = print_array (print_array print_float)

let pretty_print_matrix m =
  let x = Array.length m and y = Array.length m.(0) in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      print_float m.(i).(j);
      print_string " "
    done;
    print_newline ()
  done
