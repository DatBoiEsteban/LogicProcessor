fun gen_bools_aux boolean [] = []
  | gen_bools_aux boolean (x::xs) =
    [[boolean] @ x] @ gen_bools_aux boolean xs
;

fun gen_bools 0 = [[]]
  | gen_bools 1 = [[true], [false]]
  | gen_bools n =
    let
      val dbn_1 = gen_bools (n-1);
      val a1 = gen_bools_aux true dbn_1;
      val a2 = gen_bools_aux false dbn_1;
    in
      a1 @ a2
    end
;
