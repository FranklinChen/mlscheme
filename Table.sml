structure Table =
struct

type key = string
type value = Value.value ref (* Because mutable *)
exception NotFound of key

type table = key -> value

fun theEmptyTable name = raise NotFound name

fun lookup table name = table name

(* return a new table with name1 mapped to value *)
fun extend name1 value table name2 =
    if name2 = name1
	then value
    else
	table name2

end
