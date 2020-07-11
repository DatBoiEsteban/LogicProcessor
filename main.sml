use "auxiliar.sml";
use "data.sml";
use "vars.sml";
use "gen_bools.sml";
use "as_vals.sml";
use "taut.sml";
use "simpl.sml";

val prop = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) );

(*val letras = vars prop;

val booleans = gen_bools (length letras);

val vals = as_vals letras booleans;

val tauto = taut prop vals;*)