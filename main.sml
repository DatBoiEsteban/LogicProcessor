datatype Proposicion =
    constante       of bool
|   variable        of string
|   negacion        of Proposicion
|   conjuncion      of Proposicion * Proposicion
|   disyuncion      of Proposicion * Proposicion
|   implicacion     of Proposicion * Proposicion
|   equivalencia    of Proposicion * Proposicion

nonfix ~:
val ~:          = negacion

infix 7 :&&:
val (op :&&:)   = conjuncion

infix 6 :||:
val (op :||:)   = disyuncion

infix 5 :=>:
val (op :=>:)   = implicacion

infix 4 :<=>:
val (op :<=>:)  = equivalencia

fun evalProp prop =
  case prop of
    constante valor
      => valor
  | negacion prop1
      => not (evalProp prop1)
  | conjuncion (prop1, prop2)
      => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 andalso valor2
          end
  | disyuncion (prop1, prop2)
      => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 orelse valor2
          end
  | implicacion (prop1, prop2)
      => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  case (valor1, valor2) of
                (true, false) => false
              | _             => true
          end
  | equivalencia (prop1, prop2)
      => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 = valor2
          end
;

val prop = (variable "p") :&&: (variable "q");