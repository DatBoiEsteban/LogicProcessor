fun pow m 0 = 1
|   pow m n =
      m * pow m (n-1)
;

fun gen_bools_list_aux boolean 0= [] 
  | gen_bools_list_aux boolean len =
    [boolean] @ gen_bools_list_aux boolean (len-1)
;

fun gen_bools_list boolean alt 0 = []
|   gen_bools_list true alt final =
      gen_bools_list_aux true alt @ gen_bools_list false alt (final - alt)
|   gen_bools_list false alt final =
      gen_bools_list_aux false alt @ gen_bools_list true alt (final - alt)
;


fun alter 1 = []
|   alter n =
      let
        val mitad = n div 2;
      in
        [mitad] @ alter mitad
      end
;

fun gen_table [] n = []
|   gen_table (x::xs) n =
      [gen_bools_list true x n] @ gen_table xs n
;


fun gen_bools n =
  let
    val potencia = pow 2 n;
    val altern = alter potencia;
  in
    gen_table altern potencia
  end



