structure Value =
struct

open Sexp

datatype value = INTval of int
    | CHARval of char
    | BOOLval of bool
    | STRINGval of string
    | NILval
    | CONSval of value ref * value ref
    | VECval of value vector
    | SYMval of string (* introduced by QUOTE *)
    | FUNCval of (value list -> value) ref
    | UNITval of unit (* When Scheme unspecified *)

fun quote (INTsexp i) = INTval i
  | quote (CHARsexp c) = CHARval c
  | quote (BOOLsexp b) = BOOLval b
  | quote (STRINGsexp s) = STRINGval s
  | quote (SYMsexp s) = SYMval s
  | quote (VECsexp v) =
    VECval (Vector.tabulate (Vector.length v,
			     fn i => quote (Vector.sub (v, i))))
  | quote NILsexp = NILval
  | quote (CONSsexp (head, tail)) =
    CONSval (ref (quote head), ref (quote tail))

(*
 * Pretty-printing
 * (stolen from Sexp)
 *
 * Cycles are not currently checked for.
 *
 * Need to improve output of chars and strings so that legal
 * Scheme syntax results.
 *)
fun pp (INTval i) = Int.toString i
  | pp (CHARval c) = "#\\" ^ (Char.toString c)
  | pp (BOOLval false) = "#f"
  | pp (BOOLval true) = "#t"
  | pp (STRINGval s) = "\"" ^ s ^ "\""
  | pp (VECval v) =
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
  | pp NILval = "()"
  | pp (CONSval (ref head, ref tail)) =
    let
	fun last (CONSval (_, ref tail)) = last tail
	  | last x = x
	fun ppTail NILval = ""
	  | ppTail (CONSval (ref head, ref tail)) =
	    " " ^ pp head ^ ppTail tail
	  | ppTail _ = "<impossible-list>"
    in
	case last tail of
	    NILval => "(" ^ pp head ^ ppTail tail ^ ")"
	  | _ => "(" ^ pp head ^ " . " ^ pp tail ^ ")"
    end
  | pp (SYMval a) = a			(* not quoted *)
  | pp (FUNCval f) = "<function>"
  | pp (UNITval ()) = "<unit>"		(* nonstandard *)

end
