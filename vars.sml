(* Parámetros: Proposición.
   Descripción: Busca las variables y las agrega a una lista.
   Retorno: Lista con las variables de la proposición.
*)
fun vars_aux prop =
    case prop of
      constante valor
      => []
    | variable valor
    => [valor]
    | negacion (prop)
    => vars_aux prop
    | conjuncion (prop1, prop2)
    => vars_aux prop1 @ vars_aux prop2
    | disyuncion (prop1, prop2)
    => vars_aux prop1 @ vars_aux prop2
    | implicacion (prop1, prop2)
    => vars_aux prop1 @ vars_aux prop2
    | equivalencia (prop1, prop2)
    => vars_aux prop1 @ vars_aux prop2
    
;

(* Parámetros: Lista.
   Descripción: Elimina los elementos repetidos de una lista.
   Retorno: Lista con las variables de la proposición sin repeticiones.
*)
fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);


(* Parámetros: Proposición.
   Descripción: Llama a la función auxiliar que retorna la lista de variables y utiliza isolate para filtrarla.
   Retorno: Lista con las variables de la proposición sin repeticiones.
*)
fun vars prop    =
    let
      val letras = vars_aux prop;
    in
      isolate letras
    end
;

(*Pruebas*)

val pruebaVar1 = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) ); (*["p"]*)
val pruebaVar2 = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "q") :&&: (variable "q")) ) ); (*["p", "q"]*)
val pruebaVar3 = ((variable "p") :||: ((~:(variable "q")) :&&: (variable "r"))); (*["p", "q", "r"]*)