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

(* FMC Temporary *)
exception Unimplemented of string
exception Impossible of string

(* Useful. *)
fun isList NILsexp = true
  | isList (CONSsexp (_, tail)) = isList tail
  | isList _ = false

fun isNameList NILsexp = true
  | isNameList (CONSsexp (SYMsexp _, tail)) = isNameList tail
  | isNameList (CONSsexp (_, tail)) = false

fun appendV NILval t = t
  | appendV (CONSval (refhead, ref tail)) t =
    CONSval (refhead, ref (appendV tail t))



(*
 * Three forms.
 * Need to eventually support the hackish internal definitions.
 * FMC currently don't check that not local.
 *)
fun valueDefine (CONSsexp (SYMsexp var, CONSsexp (e, NILsexp))) =
    (* (define <variable> <expression> *)
    UNITval (Globals.extendGlobalTable var (ref (theMeaning e)))
  | valueDefine (CONSsexp (CONSsexp (SYMsexp name,
				     rest as CONSsexp _),
			   body as CONSsexp _)) =
    if isList rest then
        (* (define (<variable> <variable>* ) <body>) *)
	raise Unimplemented
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
    quote e
  | valueQuote e =
    raise Impossible "quote - should be (quote <datum>)"

and valueQuasiQuote (CONSsexp (e, NILsexp)) table =
    quasiQuote e table
  | valueQuasiQuote e _ =
    raise Impossible "quote - should be (quasiquote <datum>)"

and quasiQuote (e as INTsexp i) _ = quote e
  | quasiQuote (e as CHARsexp c) _ = quote e
  | quasiQuote (e as BOOLsexp b) _ = quote e
  | quasiQuote (e as STRINGsexp s) _ = quote e
  | quasiQuote (e as NILsexp) _ = quote e
  | quasiQuote (e as SYMsexp s) _ = quote e
  | quasiQuote (VECsexp v) table =
    VECval (Vector.tabulate (Vector.length v,
			     fn i => quasiQuote (Vector.sub (v, i))
			                        table))
  | quasiQuote (CONSsexp (SYMsexp "unquote", CONSsexp (tail, NILsexp)))
    table =
    meaning tail table
  | quasiQuote (CONSsexp (CONSsexp (SYMsexp "unquote-splicing",
				    CONSsexp (splice, NILsexp)),
			  tail)) table =
    appendV (meaning splice table) (quasiQuote tail table)
(*fmc
  | quasiQuote (CONSsexp (SYMsexp "quasiquote", tail)) =
    quasiQuote (quasiQuote tail))
*)
  | quasiQuote (CONSsexp (head, tail)) table =
    CONSval (ref (quasiQuote head table),
	     ref (quasiQuote tail table))

and valueIdentifier e table  = !(Table.lookup table e)

(* (set! <variable> <expression>) *)
and valueSet (CONSsexp (SYMsexp var, CONSsexp (e, NILsexp))) table =
    UNITval (Table.lookup table var := meaning e table)
  | valueSet e _ = raise BadSet e

(*
 * Need to handle three kinds of lambda.
 *)
and valueLambda (e as CONSsexp (vars as CONSsexp _, body as CONSsexp _))
    table =
    if isList body then
	if isList vars then		(* (lambda (<variable>+) body) *)
	    if isNameList vars then
		FUNCval (ref (fn args => beglis
			      body
			      (multiExtend vars
			       (boxAll args)
			       table)))
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
    if isList body then
	FUNCval (ref (fn _ => beglis body table))
    else
	raise BadLambda e
  | valueLambda (e as CONSsexp (SYMsexp var, body as CONSsexp _)) table =
    if isList body then
	raise Unimplemented "(lambda variable body)"
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

and boxAll args = map ref args

(* return new table *)
and multiExtend NILsexp [] table = table
  | multiExtend (CONSsexp (SYMsexp name, names)) (cell::cells)
    table =
    Table.extend name cell (multiExtend names cells table)
  | multiExtend _ _ _ = raise Impossible
                              "multiExtend - names and cells not same length"

(* Call by value, left to right args *)
and valueApplication func args table =
    if isList args then
	(case meaning func table of
	     (FUNCval (ref f)) => f (evlis args table)
	   | _ => raise BadApplication func)
    else
	raise BadApplication args

(*
 * Map list of expressions to list of values.
 * Note that evaluation is done left to right.
 * FMC may need revise in light of other lambda forms.
 *)
and evlis args table = mapStoV (fn a => meaning a table) args

and mapStoV f NILsexp = []
  | mapStoV f (CONSsexp (head, tail)) =
    f head :: mapStoV f tail




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
and valueLetcc (e as CONSsexp (SYMsexp name, body as CONSsexp _)) table =
    if isList body then
	SMLofNJ.Cont.callcc
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

and value (CONSsexp (SYMsexp "define", x)) = valueDefine x
  | value e = theMeaning e

and expressionToAction (e as CONSsexp (head, tail)) =
    if isList e then
	listToAction head tail
    else
	raise BadExpression e
  | expressionToAction e = atomToAction e

and atomToAction (e as INTsexp i) _ _ = quote e
  | atomToAction (e as CHARsexp c) _ _ = quote e
  | atomToAction (e as BOOLsexp b) _ _ = quote e
  | atomToAction (e as STRINGsexp s) _ _ = quote e
  | atomToAction (e as VECsexp v) _ _ = quote e
  | atomToAction (e as NILsexp) _ _ = quote e

  (* Add primitives here. *)
  | atomToAction (SYMsexp "cons") _ _ = makePrimFunc2 "cons" primCons
  | atomToAction (SYMsexp "car") _ _ = makePrimFunc1 "car" primCar
  | atomToAction (SYMsexp "cdr") _ _ = makePrimFunc1 "cdr" primCdr
  | atomToAction (SYMsexp "null?") _ _ = makePrimFunc1 "null?" primNull
  | atomToAction (SYMsexp "eq?") _ _ = makePrimFunc2 "eq?" primEq
  | atomToAction (SYMsexp "atom?") _ _ = makePrimFunc1 "atom?" primAtom
  | atomToAction (SYMsexp "zero?") _ _ = makePrimFunc1 "zero?" primZero
  | atomToAction (SYMsexp "add1") _ _ = makePrimFunc1 "add1" primAdd1
  | atomToAction (SYMsexp "sub1") _ _ = makePrimFunc1 "sub1" primSub1
  | atomToAction (SYMsexp "number?") _ _ = makePrimFunc1 "number?" primNumber

  | atomToAction (SYMsexp s) _ table = valueIdentifier s table

(*
 * Need to add extra builtins, and also support macros.
 * Then would use third argument??
 *)
and listToAction (SYMsexp "quote") e _ _ = valueQuote e
(*fmc*)
  | listToAction (SYMsexp "quasiquote") e _ table = valueQuasiQuote e table
  | listToAction (SYMsexp "lambda") e _ table = valueLambda e table
  | listToAction (SYMsexp "letcc") e _ table = valueLetcc e table
  | listToAction (SYMsexp "set!") e _ table = valueSet e table
  | listToAction (SYMsexp "if") e _ table = valueIf e table
  | listToAction func args _ table = valueApplication func args table



(*
 * Primitives
 *
 * Need to clean up by separating out intended arities
 *)
and makePrim1 name f =
    fn [arg1] => f arg1
     | _ => raise Arity (1, name)
and makePrim2 name f =
    fn [arg1, arg2] => f (arg1, arg2)
     | _ => raise Arity (2, name)

and makePrimFunc1 name f = (FUNCval (ref (makePrim1 name f)))
and makePrimFunc2 name f = (FUNCval (ref (makePrim2 name f)))

and primCons (a, b) =  CONSval (ref a, ref b)
and primCar (CONSval (ref a, _)) = a
  | primCar _ = raise TypeError "car"
and primCdr (CONSval (_, ref b)) = b
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
