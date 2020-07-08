fun vars_aux prop =
    case prop of
      variable valor
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

fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);

fun vars prop =
    let
      val letras = vars_aux prop;
    in
      isolate letras
    end
;