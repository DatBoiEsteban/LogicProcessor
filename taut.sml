fun is_var var str =
    if var = str then true else false
;

fun get_bool var (x::xs) =
    let
      val (str:string, boolean:bool) = x;
    in
      if is_var var str then boolean else get_bool var xs
    end
;

fun gen_prop prop vals =
    case prop of
      variable valor
      =>  let
          val boolean = get_bool valor vals;
        in
          constante boolean
        end
    | negacion prop1
    => negacion (gen_prop prop1 vals)
    | conjuncion (prop1, prop2)
    => let val valor1 = gen_prop prop1 vals
        and valor2 = gen_prop prop2 vals
      in 
        conjuncion (valor1, valor2)
      end
    | disyuncion (prop1, prop2)
    => let val valor1 = gen_prop prop1 vals 
        and valor2 = gen_prop prop2 vals
      in  
        disyuncion (valor1, valor2)
      end
    | implicacion (prop1, prop2)
    => let val valor1 = gen_prop prop1 vals
        and valor2 = gen_prop prop2 vals
      in
        implicacion (valor1, valor2)
      end
    | equivalencia (prop1, prop2)
    => let val valor1 = gen_prop prop1 vals
        and valor2 = gen_prop prop2 vals
      in
        equivalencia (valor1, valor2)
      end
;

fun taut prop [] = true
  |   taut prop (x::xs) =
    let
      val newProp = gen_prop prop x;
    in
      evalProp newProp andalso taut prop xs
    end
;