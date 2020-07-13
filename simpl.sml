(* Parámetros: Toma como valor una proposición. 
   Descripción: Simplifica la proposición en caso de encontrar concordancia con alguna regla establecida.
   Retorno: Retorna la preposición simplificada.
*)
fun simpl_aux prop =
    let
      val a = Absorcion_avanzada prop;
      val b = Absorcion a;
      val c = Doble_negacion b;
      val d = Negacion c;
      val e = Dominacion d;
      val f = Neutro e;
      val g = Or f;
      val h = And g;
      val i = Implicacion h;
      val j = Idempotencia i;
      val k = Inversos j ;
    in
      k
    end
and Absorcion_avanzada prop = (* Pv(¬P^Q) ≡ PvQ -----  P^(¬PvQ) ≡ P^Q  y sus variaciones *)
    case prop of
      disyuncion (a, conjuncion (negacion(b), c)) 
      =>
        if
          a=b
        then
          disyuncion (simpl_aux a,simpl_aux c)
        else
          disyuncion ( simpl_aux a, simpl_aux (conjuncion (simpl_aux (negacion(simpl_aux b)),simpl_aux c)))
    | disyuncion (a, conjuncion (b, negacion(c)))
    =>
      if
        a=c
      then
        disyuncion (simpl_aux a,simpl_aux b)
      else
        disyuncion (simpl_aux a, simpl_aux (conjuncion (simpl_aux b, simpl_aux (negacion(simpl_aux c)))))
    | disyuncion (conjuncion(negacion(a), b), c)
    =>
      if
        c=a
      then
        disyuncion (simpl_aux b,simpl_aux c)
      else
        disyuncion (simpl_aux (conjuncion(simpl_aux (negacion(simpl_aux a)),simpl_aux b)),simpl_aux c)
    | disyuncion (conjuncion(a, negacion(b)), c)
    =>
      if
        c=b
      then
        disyuncion(simpl_aux a,simpl_aux c)
      else
        disyuncion ( simpl_aux (conjuncion(simpl_aux a,simpl_aux (negacion(simpl_aux b)))),simpl_aux c)
    | conjuncion (a, disyuncion(negacion(b), c))
    =>
      if
        a=b
      then
        conjuncion (simpl_aux a,simpl_aux c)
      else
        conjuncion (simpl_aux a, simpl_aux (disyuncion(simpl_aux (negacion(simpl_aux b)),simpl_aux c)))
    | conjuncion (a, disyuncion (b, negacion(c)))
    =>
      if
        a=c
      then
        conjuncion (simpl_aux a,simpl_aux b)
      else
        conjuncion (simpl_aux a, simpl_aux (disyuncion (simpl_aux b,simpl_aux (simpl_aux (negacion(simpl_aux c))))))
    | conjuncion(disyuncion(negacion(a), b), c)
    =>
      if
        c=a
      then
        conjuncion(simpl_aux b,simpl_aux c)
      else
        conjuncion(simpl_aux (disyuncion(simpl_aux (simpl_aux (negacion(simpl_aux a))), simpl_aux b)), simpl_aux c)
    | conjuncion (disyuncion(a, negacion(b)), c)
    =>
      if
        c=b
      then
        conjuncion(simpl_aux a,simpl_aux c)
      else
        conjuncion ( simpl_aux(disyuncion(simpl_aux a,simpl_aux (negacion(simpl_aux b)))), simpl_aux c)
    | _ => prop
and Absorcion prop =  (* Pv(P^Q) ≡ P -----  P^(PvQ) ≡ P  y sus variaciones *)
    case prop of
      conjuncion ( a, disyuncion ( b, c))
      =>
        if
          a=b
          orelse
          a=c
        then
          a
        else
          conjuncion ( simpl_aux a, simpl_aux (disyuncion ( simpl_aux b, simpl_aux c)))
    | conjuncion (disyuncion( a, b), c)
    =>
      if
        a=c
        orelse
        b=c
      then
        c
      else
        conjuncion (simpl_aux (disyuncion( simpl_aux a, simpl_aux b)), simpl_aux c)
    | disyuncion ( a, conjuncion ( b, c))
    =>
      if
        a=b
        orelse
        a=c
      then
        a
      else
        disyuncion ( simpl_aux a, simpl_aux (conjuncion ( simpl_aux b, simpl_aux c)))
    | disyuncion (conjuncion( a,  b),  c)
    =>
      if
        a=c
        orelse
        b=c
      then
        c
      else
        disyuncion ( simpl_aux (conjuncion( simpl_aux a, simpl_aux b)), simpl_aux c)
    | _ => prop
and Doble_negacion prop = (* ¬¬P ≡ P *)
    case prop of
      negacion (negacion ( a))
      =>
        a
    | _ => prop
and Negacion prop = (* ¬(P) ≡ ¬P *)
    case prop of
      negacion (constante a)
      => 
        constante (not a)
    | negacion (a)
    =>
      negacion ( simpl_aux a)
    | _ => prop
and Dominacion prop = (* P^False ≡ False ----- PvTrue ≡ True y sus variaciones *)
    case prop of
      conjuncion (constante false, a)
      =>
        constante false
    | conjuncion (a, constante false)
    =>
      constante false
    | disyuncion (constante true, a)
    =>
      constante true
    | disyuncion (a, constante true)
    =>
      constante true
    | _ => prop
and Neutro prop = (* PvFalse ≡ P ----- P^True ≡ P y sus variaciones *)
    case prop of
      conjuncion (constante true, a) 
      =>
        simpl_aux a
    | conjuncion (a, constante true)
    =>
      simpl_aux a
    | disyuncion (constante false, a)
    =>
      simpl_aux a
    | disyuncion (a, constante false)
    =>
      simpl_aux a
    | _ => prop
and Or prop =  (* PvQ *)
    case prop of
      disyuncion (constante a, constante b)
      =>
        if
          a orelse b
        then
          constante true
        else
          constante false
    | _ => prop
and And prop = (* P^Q *)
    case prop of
      conjuncion (constante a, constante b)
      =>
        if
          a andalso b
        then
          constante true
        else
          constante false
    | _ => prop
and Implicacion prop = (* False=>P ≡ True ----- True=>P ≡ P *)
    case prop of
      implicacion (constante false, a)
      =>
        constante true
    | implicacion (constante true, a)
    =>
      simpl_aux a
    | _ => prop
and Idempotencia prop = (* PvP ≡ P -----  P^P ≡ P *)
    case prop of
      conjuncion ( a, b)
      => 
        if
          a = b
        then
          a
        else
          conjuncion (simpl_aux a, simpl_aux b)
    | disyuncion ( a, b)
    =>
      if
        a=b
      then
        a
      else
        disyuncion ( simpl_aux a, simpl_aux b)
    | _ => prop
and Inversos prop = (* Pv¬P ≡ True -----  P^¬P ≡ False *)
    case prop of
      conjuncion ( a, negacion ( b ))
      =>
        if
          a = b
        then
          constante false
        else
          conjuncion ( simpl_aux a, simpl_aux (negacion (simpl_aux b )))
    | conjuncion (negacion ( a ),  b)
    =>
      if
        a=b
      then
        constante false
      else
        conjuncion (simpl_aux (negacion ( simpl_aux a )), simpl_aux b)
    | disyuncion ( a, negacion ( b ))
    =>
      if
        a=b
      then
        constante true
      else
        disyuncion ( simpl_aux a, simpl_aux (negacion ( simpl_aux b )))
    | disyuncion (negacion ( a ), b)
    =>
      if
        a=b
      then
        constante true
      else
        disyuncion ( simpl_aux (negacion ( simpl_aux a )), simpl_aux b)
    | _ => prop
;

(* Parámetros: Toma como valores dos veces la misma proposición. 
   Descripción: Controla las simplificaciones que se le realicen a la preposición, verifica si se ha alcanzado el máximo de simplificaciones posibles.
   Retorno: La expresión más simplificada.
*)
fun simpl_aux2 prop last = 
    let
      val new_prop = simpl_aux prop;
    in
      if
        new_prop = last
      then
        last
      else
        simpl_aux2 new_prop prop
      end     
;

(* Parámetros: Toma una proposición.
   Descripción: Utliza a la función auxiliar simpl_aux2 para realizar la simplificación de la proposición lógica.
   Retorno: El resultado de simpl_aux2.
*)
fun simpl prop = simpl_aux2 prop prop
;


(*Pruebas*)

val prop2  = ((variable "p") :||: (variable "p")) (* ejemplo de idempotencia *)
(* variable "p" *)

val prop3  = ((variable "p") :&&: (variable "p")) (* ejemplo de idempotencia *)
(* variable "p" *)

val prop4  = ((variable "p") :&&: (constante true)) (* ejemplo de neutro *)
(* variable "p" *)

val prop5  = ((variable "p") :||: (constante false)) (* ejemplo de neutro *)
(* variable "p" *)

val prop6  = ((variable "p") :||: (~: (variable "p")) ) (* ejemplo de inverso *)
(* constante true *)

val prop7  = ((variable "p") :&&: (~: (variable "p")) ) (* ejemplo de inverso *)
(* constante false *)

val prop8  = ((variable "p") :||: (constante true)) (* ejemplo de dominacion *)
(* constante true *)

val prop9  = ((variable "p") :&&: (constante false)) (* ejemplo de dominacion *)
(* constante false *)

val prop10 = (((variable "p") :&&: (variable "q")) :||: (variable "p")) (* ejemplo de absorcion *)
(* variable "p" *)

val prop11 = ((variable "p") :&&: ((variable "p") :||: (variable "q"))) (* ejemplo de absorcion *)
(* variable "p" *)

val prop12 = ((variable "p") :||: ((~:(variable "p")) :&&: (variable "q"))) (* ejemplo de absorcion compleja *)
(* disyuncion(variable "p", variable "q") *)

val prop13 = ((variable "p") :&&: ((~:(variable "p")) :||: (variable "q"))) (* ejemplo de absorcion compleja *)
(*conjuncion(variable "p", variable "q")*)

val prop14 = ((constante true) :=>: (variable "p")) (* ejemplo de implica con constantes *)
(* variable "p" *)

val prop15 = ((constante false) :=>: (variable "p")) (* ejemplo de implica con constantes *)
(* constante true *)

val pruebaSimplMix = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) ); 
(* constante false *)