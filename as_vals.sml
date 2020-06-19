fun as_vals_aux var [] = []
|   as_vals_aux var (x::xs) =
      [(var, x)] @ as_vals_aux var xs
;

fun as_vals [] [] = []
|   as_vals (x::xs) (y::ys) =
      [as_vals_aux x y] @ as_vals xs ys
;
