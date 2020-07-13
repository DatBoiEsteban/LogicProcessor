(* Parámetro: Toma dos strings.
   Descripción: Verifica que ambos strings sean iguales.
   Retorno: Un true en caso de que sí sean iguales o un false en caso contario.
*)
fun es_var var str =
    if var = str then true else false
;

(* Parámetros: Toma un string y una lista.
   Descripción: Busca el valor booleano que corresponde a la variable que fue encontrada.
   Retorno: El valor booleano de la variable.
*)
fun get_bool _ []        = false
  | get_bool var (x::xs) =
    let
      val (str:string, boolean:bool) = x;
    in
      if es_var var str then boolean else get_bool var xs
    end
;

(* Parámetros: Toma como valores una proposición y la lista. 
   Descripción: Realiza la evaluación de la preposición reemplazando las variables con una combinación de valores booleanos.
   Retorno: True o False dependiendo del resultado de evaluar una determinada asiganción de valores booleanos en las variables de la preposición.
*)
fun evalProp prop vals =
    case prop of
      constante valor
      => valor
    | variable valor
      => get_bool valor vals
    | negacion prop1
      => not (evalProp prop1 vals)
    | conjuncion (prop1, prop2)
      => let
          val valor1 = evalProp prop1 vals
          and valor2 = evalProp prop2 vals
         in
          valor1 andalso valor2
         end
    | disyuncion (prop1, prop2)
      => let 
          val valor1 = evalProp prop1 vals
          and valor2 = evalProp prop2 vals
         in
          valor1 orelse valor2
         end
    | implicacion (prop1, prop2)
      => let 
          val valor1 = evalProp prop1 vals
          and valor2 = evalProp prop2 vals
         in
          case (valor1, valor2) of
            (true, false) => false
          | _             => true
         end
    | equivalencia (prop1, prop2)
      => let
          val valor1 = evalProp prop1 vals
          and valor2 = evalProp prop2 vals
         in  
          valor1 = valor2
         end
;

(* Parámetros: Toma una proposición y una lista de listas formadas por variables y valores booleano (generada en as_vals).
   Descripción: Utiliza evalProp para evaluar cada una de las sublistas que contiene y determinar si todas dan true, lo que corresponde a una tautología.
   Retorno: El resultado de la evaluación de todas la sublistas en evalProp, en caso de que compruebe que es una tautología imprime la proposición 
   con la función bonita seguido de la leyenda "Sí es una tautologia". En caso contrario imprime una de las evaluaciones de evalProp que 
   causa la contradicción seguido de la leyenda "No es una tautología".
*)
fun taut_aux prop []      = bonita prop  ^ " Sí es una tautología"
  | taut_aux prop (x::xs) =
    let
      val evaluado = evalProp prop x;
    in
      if evaluado
      then
        taut_aux prop xs
      else
        bonita prop ^ " No es una tautología, debido a : " ^ str_a x  
    end
;

(* Parámetros: Toma una proposición.
   Descripción: Ejecuta vars, gen_bools, as_vals y sus auxiliares para poder determinar si una proposición es una tautología.
   Retorno: El resultado de taut_aux.
*)
fun taut prop =
  let
    val rvars = vars prop;
    val rgen_bools = gen_bools (length rvars);
    val ras_vals = as_vals rvars rgen_bools;
  in
    taut_aux prop ras_vals
  end
;

(*Pruebas*)

val pruebaTaut  = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) );
(* "¬((¬p ^ ¬p) v (p ^ p)) No es una tautología, debido a : [ ( 'p', True ) ]" *)

val pruebaTaut2 = ((variable "p") :||: (~:(variable "p")));
(* "(p v ¬p) Sí es una tautología" *)
