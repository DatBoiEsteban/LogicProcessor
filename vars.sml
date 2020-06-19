fun vars prop =
  case prop of
    variable valor
      => [valor]
    | conjuncion (prop1, prop2)
      => vars prop1 @ vars prop2
    | disyuncion (prop1, prop2)
      => vars prop1 @ vars prop2
    | implicacion (prop1, prop2)
      => vars prop1 @ vars prop2
    | equivalencia (prop1, prop2)
      => vars prop1 @ vars prop2
;


val prop =  (variable "p") :&&: (variable "p");
fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);