use "data.sml";
use "auxiliar.sml";
use "vars.sml";
use "gen_bools.sml";
use "as_vals.sml";
use "taut.sml";
use "simpl.sml";

val prop   = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) ); 


