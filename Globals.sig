signature Globals =
sig

val extendGlobalTable : string -> Value.value -> unit
val lookupInGlobalTable : Table.table

end
