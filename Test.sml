(* For testing. *)

val ps = #1 o Join.parseString

val pps = Sexp.pp o ps
val va = Interpret.value o ps
val ip = Value.pp o Interpret.value o ps

val foo = "(if (zero? #f) '5 '2)"
val foo' = ip foo

val foo1 = "'(1 2 3 4 #f)"
val foo1' = ip foo1

val foo2 = "#(1 2 3 4 #f (a b))"
val foo2' = ip foo2

val foo3 = "(define foo 666)"
val foo3' = ip foo3

val foo4 = "`(1 ,foo)"
val foo4' = ip foo4

val foo5 = "(define bar '(1 2 3))"
val foo5' = ip foo5

val foo6 = "`(13 ,foo ,@bar me)"
val foo6' = ip foo6
