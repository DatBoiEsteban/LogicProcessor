fun as_vals_aux var [] = []
  |   as_vals_aux var (x::xs) =
    [(var, x)] @ as_vals_aux var xs
;

fun parse_as_vals_aux [] n = []
  |   parse_as_vals_aux (x::xs) n =
    [List.nth (x, n)] @ parse_as_vals_aux xs n
;

fun parse_as_vals vals 0 = [parse_as_vals_aux vals 0]
  |   parse_as_vals vals n =
    [parse_as_vals_aux vals n] @ parse_as_vals vals (n-1)
;

(*Letras y booleans en ese orden*)
fun gen_vals [] [] = []
  |   gen_vals (x::xs) (y::ys) =
    [as_vals_aux x y] @ gen_vals xs ys
;

fun as_vals letras booleans =
    let
      val vals = gen_vals letras booleans;
      val len = length (List.nth (vals, 0)) -1;
    in
      parse_as_vals vals len
    end
;