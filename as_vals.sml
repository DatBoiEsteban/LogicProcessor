(* Parámetros: Dos listas.
   Descripción: Une los elementos con el mismo índice de dos listas
   Retorno: Una lista de pares ordenados.
*)
fun zip (x::xs) (y::ys) = [(x, y)] @ zip xs ys
  | zip _ _             = []
;

(* Parámetros: Lista de las variables de la proposición (string) y la lista de las combinaciones booleanas.
   Descripción: Utiliza la función zip para combinar las variables de la proposición con las diferentes combinaciones de valores booleanos.
   Retorno: Lista de las variables asociadas a combinaciones de valores booleanos.
*)
fun as_vals letras []      = []
  | as_vals letras (x::xs) =
    zip letras x :: as_vals letras xs
;

(*Prueba*)

val rvars = vars ((variable "p") :||: ((~:(variable "q")) :&&: (variable "r")));

val rgen_bools = gen_bools (length rvars);

val prueba_as_vals = as_vals rvars rgen_bools;

   (*[[("p", true), ("q", true), ("r", true)],
     [("p", true), ("q", true), ("r", false)],
     [("p", true), ("q", false), ("r", true)],
     [("p", true), ("q", false), ("r", false)],
     [("p", false), ("q", true), ("r", true)],
     [("p", false), ("q", true), ("r", false)],
     [("p", false), ("q", false), ("r", true)],
     [("p", false), ("q", false), ("r", false)]]
    *)

val rvars2 = vars ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) );

val rgen_bools2 = gen_bools (length rvars2);

val prueba_as_vals2 = as_vals rvars2 rgen_bools2;

    (*
    [[("p", true)], [("p", false)]]
    *)


