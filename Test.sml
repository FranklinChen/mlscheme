(* For testing. *)

val ps = #1 o Join.parseString
val foo = "(if (zero? #f) '5 '2)"
val foo1 = "'(1 2 3 4 #f)"
val foo2 = "#(1 2 3 4 #f (a b))"

val pps = Sexp.pp o ps
val va = Interpret.value o ps
val ip = Value.pp o Interpret.value o ps
