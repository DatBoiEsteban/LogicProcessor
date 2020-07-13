(* Parámetros: Boolean y lista.
   Descripción: Realiza un append de un valor booleano a una lista de booleanos.
   Retorno: Una lista de listas de largo 2^n y cada una de las listas que la compone de largo de la cantidad de variables.
*)
fun gen_bools_aux boolean      [] = []
  | gen_bools_aux boolean (x::xs) =
    [boolean :: x] @ gen_bools_aux boolean xs
;

(* Parámetros: Integer.
   Descripción: Se llama a sí misma para obtener la lista inicial y utiliza la función auxiliar para ir haciendo las diferentes combinaciones de valores booleanos.
   Retorno: Una lista de listas con las diferentes combinaciones de valores booleanos que se pueden formar con n variables.
*)
fun gen_bools 0 = [[]]
  | gen_bools 1 = [[true], [false]]
  | gen_bools n =
    let
      val dbn_1 = gen_bools (n-1);
      val a1    = gen_bools_aux true dbn_1;
      val a2    = gen_bools_aux false dbn_1;
    in
      a1 @ a2
    end
;

(*Pruebas*)

val pruebaGenBools    = ["p","q"];
val pruebaGenBoolslen = length(pruebaGenBools); 
gen_bools pruebaGenBoolslen;                   (*[[true, true], 
                                                  [true, false], 
                                                  [false, true], 
                                                  [false, false]]*)

val pruebaGenBools2    = ["p"];
val pruebaGenBoolslen2 = length(pruebaGenBools2);
gen_bools pruebaGenBoolslen2;                 (*[[true], [false]]*)

val pruebaGenBools3    = ["p","q","r"];
val pruebaGenBoolslen3 = length(pruebaGenBools3);
gen_bools pruebaGenBoolslen3;                 (*[[true, true, true], 
                                                 [true, true, false], 
                                                 [true, false, true],
                                                 [true, false, false], 
                                                 [false, true, true], 
                                                 [false, true, false],
                                                 [false, false, true], 
                                                 [false, false, false]]*)

