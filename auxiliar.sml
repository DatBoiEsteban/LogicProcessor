(* Parámetros: Toma un valor booleano.
   Descripción: Convierte un valor booleano a su equivalente en string.
   Retorno: El string del valor booleano.
*)
fun bool_a_str true = "True"
  | bool_a_str false = "False"
;

(* Parámetros: Toma una lista.
   Descripción: Convierte las tuplas de la lista en string.
   Retorno: Las tuplas de la lista en string.
*)
fun str_b [] = ""
  | str_b (x::xs) =
    let
      val (str:string, boolean:bool) = x;
    in
      "( '" ^ str ^ "', " ^ bool_a_str boolean ^ " )" ^  (str_b xs)
    end
;

(* Parámetros: Toma una lista. 
   Descripción: Llama a la función str_b para convertir la lista en string y agrega formato con "[ ]"
   Retorno: La lista en string con "[ ]".
*)
fun str_a []    = "Evaluó constante False"
  | str_a lista =
    "[ " ^ str_b lista ^ " ]" 
;

(* Parámetros: Toma una proposición.
   Descripción: Imprime la proposición con un formato bonito, utilizando los símbolos dados por las disposiciones de la asignación.
   Retorno: La preposición con formato bonito.
*)
fun bonita prop =
    case prop of
    constante valor
    => bool_a_str valor
  | variable valor
    => valor
  | negacion prop1
    =>  "¬" ^ bonita prop1  
  | conjuncion (prop1, prop2)
    =>  "(" ^ bonita prop1 ^ " ^ " ^ bonita prop2 ^ ")"
  | disyuncion (prop1, prop2)
    =>  "(" ^ bonita prop1 ^ " v " ^ bonita prop2 ^ ")" 
  | implicacion (prop1, prop2)
    =>  "(" ^ bonita prop1 ^ " ⇒ " ^ bonita prop2 ^ ")"
  | equivalencia (prop1, prop2)
    =>  "(" ^ bonita prop1 ^ " ⇔ " ^ bonita prop2 ^ ")"
  | _ => ""

(*Pruebas*)

val prop   = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) ); (* "¬((¬p ^ ¬p) v (p ^ p))" *)








