structure Globals =
struct

val globalTable : Table.table ref = ref Table.theEmptyTable

fun extendGlobalTable key value =
    globalTable := Table.extend key value (!globalTable)

fun lookupInGlobalTable name = Table.lookup (!globalTable) name

end
