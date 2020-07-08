fun zip [] [] = []
  | zip [] _ = []
  | zip _ [] = []
  | zip (x::xs) (y::ys) = [(x, y)] @ zip xs ys
;
fun as_vals letras [] = []
  | as_vals letras (x::xs) =
    zip letras x :: as_vals letras xs
;