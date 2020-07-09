fun simpl_aux prop =
    case prop of
      (* encontro constante *)
      constante valor
      =>
        constante valor
      (* encontro variable *)
    | variable valor
    =>
      variable valor
    (* Doble Negacion *)
    | negacion (negacion ( a))
    =>
      a
    (* Negacion *)
    | negacion (constante a)
    => 
      constante (not a)
    | negacion (a)
    =>
      negacion ( simpl_aux a)
    (* Implicacion *)
    | implicacion (constante false, a)
    =>
      constante true
    | implicacion (constante true, a)
    =>
      simpl_aux a
    (* AND normal *)
    | conjuncion (constante a, constante b)
    =>
      if
        a andalso b
      then
        constante true
      else
        constante false
    (* OR normal *)
    | disyuncion (constante a, constante b)
    =>
      if
        a orelse b
      then
        constante true
      else
        constante false
    (* Neutro *)
    | conjuncion (constante true, a) 
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
    (* Dominacion *)
    | conjuncion (constante false, a)
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
    (* Inversos *)
    | conjuncion ( a, negacion ( b ))
    =>
      if
        a = b
      then
        constante false
      else
        conjuncion ( simpl_aux a, negacion (simpl_aux b ))
    | conjuncion (negacion ( a ),  b)
    =>
      if
        a=b
      then
        constante false
      else
        conjuncion (negacion ( simpl_aux a ), simpl_aux b)
    | disyuncion ( a, negacion ( b ))
    =>
      if
        a=b
      then
        constante true
      else
        disyuncion ( simpl_aux a, negacion ( simpl_aux b ))
    | disyuncion (negacion ( a ), b)
    =>
      if
        a=b
      then
        constante true
      else
        disyuncion (negacion ( simpl_aux a ), simpl_aux b)
    (* Idempotencia *)
    | conjuncion ( a,  b)
    => 
      if
        a = b
      then
        a
      else
        conjuncion ( a,  b)
    | disyuncion ( a,  b)
    =>
      if
        a=b
      then
        a
      else
        disyuncion ( simpl_aux a, simpl_aux b)
    (* Absorcion *)
    | conjuncion ( a, disyuncion ( b, c))
    =>
      if
        a=b
        orelse
        a=c
      then
        a
      else
        conjuncion ( simpl_aux a, disyuncion ( simpl_aux b, simpl_aux c))
    | conjuncion (disyuncion( a, b), c)
    =>
      if
        a=c
        orelse
        b=c
      then
        c
      else
        conjuncion (disyuncion( simpl_aux a, simpl_aux b), simpl_aux c)
    | disyuncion ( a, conjuncion ( b, c))
    =>
      if
        a=b
        orelse
        a=c
      then
        a
      else
        disyuncion ( simpl_aux a, conjuncion ( simpl_aux b, simpl_aux c))
    | disyuncion (conjuncion( a,  b),  c)
    =>
      if
        a=c
        orelse
        b=c
      then
        c
      else
        disyuncion (conjuncion( simpl_aux a, simpl_aux b), simpl_aux c)
    (* Absorcion Avanzada *)
    | disyuncion (a, conjuncion (negacion(b), c))
    =>
      if
        a=b
      then
        disyuncion (a, c)
      else
        disyuncion ( simpl_aux a, conjuncion (negacion(simpl_aux b),simpl_aux c))
    | disyuncion (a, conjuncion (b, negacion(c)))
    =>
      if
        a=c
      then
        disyuncion (a,b)
      else
        disyuncion (simpl_aux a, conjuncion (simpl_aux b, negacion(simpl_aux c)))
    | disyuncion (conjuncion(negacion(a), b), c)
    =>
      if
        c=a
      then
        disyuncion (b,c)
      else
        disyuncion (conjuncion(negacion(simpl_aux a),simpl_aux b),simpl_aux c)
    | disyuncion (conjuncion(a, negacion(b)), c)
    =>
      if
        c=b
      then
        disyuncion(a,c)
      else
        disyuncion (conjuncion(simpl_aux a, negacion(simpl_aux b)),simpl_aux c)
    | conjuncion (a, disyuncion(negacion(b), c))
    =>
      if
        a=b
      then
        conjuncion (a,c)
      else
        conjuncion (simpl_aux a, disyuncion(negacion(simpl_aux b),simpl_aux c))
    | conjuncion (a, disyuncion (b, negacion(c)))
    =>
      if
        a=c
      then
        conjuncion (a, b)
      else
        conjuncion (simpl_aux a, disyuncion (simpl_aux b, negacion(simpl_aux c)))
    | conjuncion(disyuncion(negacion(a), b), c)
    =>
      if
        c=a
      then
        conjuncion(b,c)
      else
        conjuncion(disyuncion(negacion(simpl_aux a),simpl_aux b),simpl_aux c)
    | conjuncion (disyuncion(a, negacion(b)), c)
    =>
      if
        c=b
      then
        conjuncion(a,c)
      else
        conjuncion (disyuncion(simpl_aux a, negacion(simpl_aux b)),simpl_aux c)
;