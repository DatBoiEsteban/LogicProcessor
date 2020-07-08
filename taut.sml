fun is_var var str =
    if var = str then true else false
;

fun get_bool _ [] = false
  | get_bool var (x::xs) =
    let
      val (str:string, boolean:bool) = x;
    in
      if is_var var str then boolean else get_bool var xs
    end
;

fun evalProp prop vals=
    case prop of
      constante valor
      => valor
    | variable valor
    => get_bool valor vals
    | negacion prop1
    => not (evalProp prop1 vals)
    | conjuncion (prop1, prop2)
    => let val valor1 = evalProp prop1 vals
        and valor2 = evalProp prop2 vals
      in  valor1 andalso valor2
      end
    | disyuncion (prop1, prop2)
    => let val valor1 = evalProp prop1 vals
        and valor2 = evalProp prop2 vals
      in  valor1 orelse valor2
      end
    | implicacion (prop1, prop2)
    => let val valor1 = evalProp prop1 vals
        and valor2 = evalProp prop2 vals
      in  case (valor1, valor2) of
          (true, false) => false
        | _             => true
      end
    | equivalencia (prop1, prop2)
    => let val valor1 = evalProp prop1 vals
        and valor2 = evalProp prop2 vals
      in  valor1 = valor2
      end
;

fun taut prop [] = true
  |   taut prop (x::xs) =
    evalProp prop x andalso taut prop xs
;