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
      val k = Inversos j;
      val l = Variable k;
      val m = Constante l;
    in
      m
    end
and Absorcion_avanzada prop =
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
and Absorcion prop =
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
and Doble_negacion prop =
    case prop of
      negacion (negacion ( a))
      =>
        a
    | _ => prop
and Negacion prop =
    case prop of
      negacion (constante a)
      => 
        constante (not a)
    | negacion (a)
    =>
      negacion ( simpl_aux a)
    | _ => prop
and Dominacion prop =
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
and Neutro prop =
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
and Or prop =
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
and And prop =
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
and Implicacion prop =
    case prop of
      implicacion (constante false, a)
      =>
        constante true
    | implicacion (constante true, a)
    =>
      simpl_aux a
    | _ => prop
and Idempotencia prop =
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
and Inversos prop =
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
and Variable prop =
    case prop of
      variable valor
      =>
        variable valor
    | _ => prop
and Constante prop =
    case prop of
      constante valor
      =>
        constante valor
    | _ => prop
;

fun simpl_aux2 prop last =
    if
      prop = last
    then
      last
    else
      let
        val new_prop = simpl_aux prop;
      in
        simpl_aux2 new_prop prop
      end
;

fun simpl (constante a) = constante a
  | simpl (variable a) = variable a
  | simpl prop = simpl_aux2 prop (constante true)
;