fun bool_to_str true = "true"
  | bool_to_str false = "false"
;

fun str_b [] = ""
  | str_b (x::xs) =
    let
      val (str:string, boolean:bool) = x;
    in
      "( '" ^ str ^ "', " ^ bool_to_str boolean ^ " )" ^  (str_b xs)
    end
;

fun str_a lista =
    "[ " ^ str_b lista ^ " ]" 
;