structure Interpret =
struct

open Sexp
open Value

(* Need to make more thorough *)
exception TypeError of string
exception Arity of int * string

exception BadExpression of sexp
exception BadSet of sexp
exception BadLambda of sexp
exception BadIf of sexp
exception BadLetcc of sexp
exception BadApplication of sexp

(* Temporary *)
exception Unimplemented of string
exception Impossible of string

(* Useful. *)
fun isList NILsexp = true
  | isList (CONSsexp (_, tail)) = isList tail
  | isList _ = false

fun isNameList NILsexp = true
  | isNameList (CONSsexp (IDsexp _, tail)) = isNameList tail
  | isNameList (CONSsexp (_, tail)) = false



(*
 * Three forms.
 * Need to eventually support the hackish internal definitions.
 *)
fun valueDefine (CONSsexp (IDsexp var, CONSsexp (e, NILsexp))) =
    (* (define <variable> <expression> *)
    UNITval (Globals.extendGlobalTable var (ref (theMeaning e)))
  | valueDefine (CONSsexp (CONSsexp (IDsexp name,
				     rest as CONSsexp _),
			   body as CONSsexp _)) =
    if isList rest
        (* (define (<variable> <variable>* ) <body>) *)
	then raise Unimplemented
	    "(define (<variable> <variable>* ) <body>)"
    else
        (* (define (<variable> <variable>* . <variable>) <body>) *)
	raise Unimplemented
	    "(define (<variable> <variable>* . <variable>) <body>)"

(* meaning *)
and theMeaning e = meaning e Globals.lookupInGlobalTable

(*
 * semantics
 * Need find why pass e also?
 *)
and meaning e table = (expressionToAction e) e table


(* actions *)

(*
 * Translate the S-expression into a value.
 *)
and valueQuote (CONSsexp (e, NILsexp)) =
    valueQuoteSexp e
  | valueQuote e =
    raise Impossible "quote - should be (quote <datum>)"

and valueQuoteSexp (INTsexp i) = INTval i
  | valueQuoteSexp (CHARsexp c) = CHARval c
  | valueQuoteSexp (BOOLsexp b) = BOOLval b
  | valueQuoteSexp (STRINGsexp s) = STRINGval s
  | valueQuoteSexp (IDsexp s) = SYMval s (* introduction *)
(*
  | valueQuoteSexp (VECsexp v) = VECval (vecStoV v)
*)
  | valueQuoteSexp NILsexp = NILval
  | valueQuoteSexp (CONSsexp (head, tail)) =
    (* deep recursive quoting *)
    CONSval (valueQuoteSexp head, valueQuoteSexp tail)

and valueIdentifier e table  = !(Table.lookup table e)

(* (set! <variable> <expression>) *)
and valueSet (CONSsexp (IDsexp var, CONSsexp (e, NILsexp))) table =
    UNITval ((Table.lookup table var) := meaning e table)
  | valueSet e _ = raise BadSet e

(*
 * Need to handle three kinds of lambda.
 *)
and valueLambda (e as CONSsexp (vars as CONSsexp _, body as CONSsexp _))
    table =
    if isList body
	then if isList vars		(* (lambda (<variable>+) body) *)
		 then if isNameList vars
			  then FUNCval
			      (fn args => beglis
			                  body
					  (multiExtend vars
					               (boxAll args)
						       table))
		      else
			  raise BadLambda e
	     else
		 (* Will need to check name dotted pair here *)
		 raise Unimplemented
		     "(lambda (<variable>+ . <variable) body)"
    else
	raise BadLambda e
  | valueLambda (e as CONSsexp (NILsexp, body as CONSsexp _)) table =
    (* (lambda () body) *)
    if isList body
	then FUNCval (fn args => beglis body table)
    else
	raise BadLambda e
  | valueLambda (e as CONSsexp (IDsexp var, body as CONSsexp _)) table =
    if isList body
	then raise Unimplemented "(lambda variable body)"
    else
	raise BadLambda e
  | valueLambda e _ = raise BadLambda e

(*
 * Return value of last expression, evaluating all expressions in order
 * for side effects.
 *
 * Assumes list.
 *)
and beglis (CONSsexp (e, NILsexp)) table = meaning e table
  | beglis (CONSsexp (e, es as CONSsexp _)) table =
    (
     meaning e table;			(* discard value *)
     beglis es table
     )

(* Need to handle improper lists? *)
and boxAll args = mapVtoL ref args
and mapVtoL f NILval = []
  | mapVtoL f (CONSval (head, tail)) =
    (f head)::(mapVtoL f tail)

(* return new table *)
and multiExtend NILsexp [] table = table
  | multiExtend (CONSsexp (IDsexp name, names)) (cell::cells)
    table =
    Table.extend name cell (multiExtend names cells table)
  | multiExtend _ _ _ = raise Impossible
                              "multiExtend - names and cells not same length"

(* Call by value, left to right args *)
and valueApplication func args table =
    if isList args
	then
	    (case meaning func table of
		 FUNCval f => f (evlis args table)
	       | _ => raise BadApplication func)
    else
	raise BadApplication args

(*
 * Map list of expressions to list of values.
 * Note that evaluation is done left to right.
 * FMC may need revise in light of other lambda forms.
 *)
and evlis args table = mapStoV (fn a => meaning a table) args

and mapStoV f NILsexp = NILval
  | mapStoV f (CONSsexp (head, tail)) =
    CONSval (f head, mapStoV f tail)




(* Two forms of "if" *)
and valueIf (CONSsexp (test, CONSsexp (t, CONSsexp (f, NILsexp)))) table =
    meaning (case meaning test table of
		 BOOLval false => f
	       | _ => t)
            table
  | valueIf (CONSsexp (test, CONSsexp (t, NILsexp))) table =
	    (case meaning test table of
		 BOOLval false => UNITval ()
	       | _ => meaning t table)
  | valueIf e _ = raise BadIf e

(*fmc*)
(* Need to do call-with-current-continuation instead *)
and valueLetcc (e as CONSsexp (IDsexp name, body as CONSsexp _)) table =
    if isList body
	then SMLofNJ.callcc
	    (fn skip => beglis body
	                       (Table.extend name
(*
				             (ref (aPrim skip))
*)
(raise Unimplemented "letcc")
					     table))
    else
	raise BadLetcc e
  | valueLetcc e _ = raise BadLetcc e



(* interpreter *)

and value (CONSsexp (IDsexp "define", x)) = valueDefine x
  | value e = theMeaning e

and expressionToAction (e as CONSsexp (head, tail)) =
    if isList e
	then listToAction head tail
    else
	raise BadExpression e
  | expressionToAction e = atomToAction e

and atomToAction (INTsexp i) _ _ = INTval i
  | atomToAction (CHARsexp c) _ _ = CHARval c
  | atomToAction (BOOLsexp b) _ _ = BOOLval b
  | atomToAction (STRINGsexp s) _ _ = STRINGval s
  | atomToAction (e as VECsexp v) _ _ = valueQuoteSexp e
  | atomToAction NILsexp _ _ = NILval

    (* Add primitives here. *)
  | atomToAction (IDsexp "cons") _ _ = FUNCval (makePrim2 "cons" primCons)
  | atomToAction (IDsexp "car") _ _ = FUNCval (makePrim1 "car" primCar)
  | atomToAction (IDsexp "cdr") _ _ = FUNCval (makePrim1 "cdr" primCdr)
  | atomToAction (IDsexp "null?") _ _ = FUNCval (makePrim1 "null?" primNull)
  | atomToAction (IDsexp "eq?") _ _ = FUNCval (makePrim2 "eq?" primEq)
  | atomToAction (IDsexp "atom?") _ _ = FUNCval (makePrim1 "atom?" primAtom)
  | atomToAction (IDsexp "zero?") _ _ = FUNCval (makePrim1 "zero?" primZero)
  | atomToAction (IDsexp "add1") _ _ = FUNCval (makePrim1 "add1" primAdd1)
  | atomToAction (IDsexp "sub1") _ _ = FUNCval (makePrim1 "sub1" primSub1)
  | atomToAction (IDsexp "number?") _ _ = FUNCval (makePrim1 "number?"
						   primNumber)

  | atomToAction (IDsexp s) _ table = valueIdentifier s table

(*
 * Need to add extra builtins, and also support macros.
 * Then would use third argument??
 *)
and listToAction (IDsexp "quote") e _ _ = valueQuote e
  | listToAction (IDsexp "lambda") e _ table = valueLambda e table
  | listToAction (IDsexp "letcc") e _ table = valueLetcc e table
  | listToAction (IDsexp "set!") e _ table = valueSet e table
  | listToAction (IDsexp "if") e _ table = valueIf e table
  | listToAction func args _ table = valueApplication func args table



(*
 * Primitives
 *
 * Need to clean up by separating out intended arities
 *)
and makePrim1 name f =
    fn (CONSval (arg1, NILval)) => f arg1
     | _ => raise Arity (1, name)
and makePrim2 name f =
    fn (CONSval (arg1, CONSval (arg2, NILval))) => f (arg1, arg2)
     | _ => raise Arity (2, name)

and primCons (a, b) =  CONSval (a, b)
and primCar (CONSval (a, _)) = a
  | primCar _ = raise TypeError "car"
and primCdr (CONSval (_, b)) = b
  | primCdr _ = raise TypeError "cdr"
and primNull NILval = BOOLval true
  | primNull _ = BOOLval false
and primEq (a, b) =
    (* Incorrect; this actually implements "equal?" *)
    raise Unimplemented "eq?"
and primAtom (CONSval _) = BOOLval false
  | primAtom _ = BOOLval true
and primZero (INTval 0) = BOOLval true
  | primZero _ = BOOLval false
and primAdd1 (INTval i) = INTval (i + 1)
  | primAdd1 _ = raise TypeError "add1"
and primSub1 (INTval i) = INTval (i - 1)
  | primSub1 _ = raise TypeError "sub1"
and primNumber (INTval _) = BOOLval true
  | primNumber _ = BOOLval false

end
