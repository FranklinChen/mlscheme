(* FMC Should really use hash table or something *)

signature Table =
sig

eqtype key
type value

exception NotFound of key

type table

val theEmptyTable : table
val lookup : table -> key -> value
val extend : key -> value -> table -> table

end
