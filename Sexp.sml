structure Sexp =
struct

datatype sexp = INTsexp of int
  | CHARsexp of char
  | BOOLsexp of bool
  | STRINGsexp of string (* "foo" *)
  | IDsexp of string (* foo *)
  | NILsexp (* () *)
  | CONSsexp of sexp * sexp (* (...), including dotted pair *)
  | VECsexp of sexp vector

(*
 * Pretty-printing
 *
 * Cycles are not currently checked for.
 *
 * Need to improve output of chars and strings so that legal
 * Scheme syntax results.
 *)
fun pp (INTsexp i) = Int.toString i
  | pp (CHARsexp c) = "#\\" ^ (Char.toString c)
  | pp (BOOLsexp false) = "#f"
  | pp (BOOLsexp true) = "#t"
  | pp (STRINGsexp s) = "\"" ^ s ^ "\""
  | pp (IDsexp s) = s
  | pp (VECsexp v) =
    (case Vector.length v of
	 0 => "#()"
       | vecLen =>
	     let
		 fun ppVec i =
		     if i < vecLen
			 then " " ^ pp (Vector.sub (v, i)) ^ ppVec (i+1)
		     else
			 ""
	     in
		 "#(" ^ pp (Vector.sub (v, 0)) ^ ppVec 1 ^ ")"
	     end)
  | pp NILsexp = "()"
  | pp (CONSsexp (head, tail)) =
    let
	fun last (CONSsexp (head, tail)) = last tail
	  | last x = x
	fun ppTail NILsexp = ""
	  | ppTail (CONSsexp (head, tail)) =
	    " " ^ pp head ^ ppTail tail
	  | ppTail _ = "<impossible-list>"
    in
	case last tail of
	    NILsexp => "(" ^ pp head ^ ppTail tail ^ ")"
	  | _ => "(" ^ pp head ^ " . " ^ pp tail ^ ")"
    end

end
