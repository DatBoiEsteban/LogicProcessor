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

exception NoEsTautologia of string;
