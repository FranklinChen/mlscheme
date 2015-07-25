signature Sexp =
sig

(* FMC need do *)
type Sexp

val isDefine : Sexp -> bool
val defineName : Sexp -> string
val defineRightSide : Sexp -> Sexp

val isQuote : Sexp -> bool
val quoteText : Sexp -> Sexp

val isIdentifier : Sexp -> bool
val identifierName : Sexp -> string

val isSet : Sexp -> bool
val setName : Sexp -> string
val setRightSide : Sexp -> Sexp

val isLambda : Sexp -> bool
val lambdaFormals : Sexp -> Sexp
val lambdaBody : Sexp -> Sexp

val isApplication : Sexp -> bool
val applicationFunction : Sexp -> Sexp
val applicationArguments : Sexp -> Sexp

val isCond : Sexp -> bool
val condLines : Sexp -> Sexp

val isLetcc : Sexp -> bool
val letccName : Sexp -> string
val letccBody : Sexp -> Sexp

end
