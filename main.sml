use "auxiliar.sml";
use "data.sml";
use "vars.sml";
use "gen_bools.sml";
use "as_vals.sml";
use "taut.sml";
use "simpl.sml";

val prop   = ( ~: (((~: (variable "p")) :&&: (~:(variable "p"))) :||: ((variable "p") :&&: (variable "p")) ) );

val prop2  = ((variable "p") :||: (variable "p")) (* ejemplo de idempotencia *)

val prop3  = ((variable "p") :&&: (variable "p")) (* ejemplo de idempotencia *)

val prop4  = ((variable "p") :&&: (constante true)) (* ejemplo de neutro *)

val prop5  = ((variable "p") :||: (constante false)) (* ejemplo de neutro *)

val prop6  = ((variable "p") :||: (~: (variable "p")) ) (* ejemplo de inverso *)

val prop7  = ((variable "p") :&&: (~: (variable "p")) ) (* ejemplo de inverso *)

val prop8  = ((variable "p") :||: (constante true)) (* ejemplo de dominacion *)

val prop9  = ((variable "p") :&&: (constante false)) (* ejemplo de dominacion *)

val prop10 = (((variable "p") :&&: (variable "q")) :||: (variable "p")) (* ejemplo de absorcion *)

val prop11 = ((variable "p") :&&: ((variable "p") :||: (variable "q"))) (* ejemplo de absorcion *)

val prop12 = ((variable "p") :||: ((~:(variable "p")) :&&: (variable "q"))) (* ejemplo de absorcion compleja *)

val prop13 = ((variable "p") :&&: ((~:(variable "p")) :||: (variable "q"))) (* ejemplo de absorcion compleja *)

val prop14 =

val prop15 = 
