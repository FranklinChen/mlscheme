functor SchemeLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Scheme_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Scheme grammar *)

open Sexp

fun dottedPair xs last =
    let
	fun aux [x] = CONSsexp (x, last)
	  | aux (x::xs) = CONSsexp (x, aux xs)
    in
	aux xs
    end

fun abbrevPrefix s e =
    CONSsexp (SYMsexp s, CONSsexp (e, NILsexp))


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\048\000\003\000\014\000\004\000\013\000\005\000\046\000\
\\006\000\012\000\007\000\011\000\008\000\010\000\009\000\009\000\
\\010\000\008\000\011\000\007\000\012\000\006\000\013\000\005\000\
\\014\000\004\000\000\000\
\\001\000\002\000\029\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\006\000\012\000\007\000\011\000\
\\008\000\010\000\009\000\009\000\010\000\008\000\011\000\007\000\
\\012\000\006\000\013\000\005\000\014\000\004\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\005\000\025\000\000\000\
\\001\000\005\000\031\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\003\000\014\000\004\000\013\000\006\000\012\000\007\000\011\000\
\\008\000\010\000\009\000\009\000\010\000\008\000\011\000\007\000\
\\012\000\006\000\013\000\005\000\014\000\004\000\000\000\
\\047\000\000\000\
\\048\000\003\000\014\000\004\000\013\000\006\000\012\000\007\000\011\000\
\\008\000\010\000\009\000\009\000\010\000\008\000\011\000\007\000\
\\012\000\006\000\013\000\005\000\014\000\004\000\000\000\
\\049\000\000\000\
\"
val actionRowNumbers =
"\003\000\007\000\012\000\011\000\
\\010\000\009\000\008\000\003\000\
\\003\000\003\000\003\000\022\000\
\\020\000\019\000\018\000\017\000\
\\016\000\004\000\022\000\005\000\
\\001\000\015\000\023\000\013\000\
\\002\000\021\000\001\000\003\000\
\\006\000\014\000\000\000"
val gotoT =
"\
\\001\000\030\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\013\000\000\000\
\\002\000\014\000\000\000\
\\002\000\015\000\000\000\
\\002\000\016\000\000\000\
\\002\000\018\000\004\000\017\000\000\000\
\\002\000\020\000\003\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\018\000\004\000\022\000\000\000\
\\000\000\
\\002\000\026\000\003\000\025\000\004\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\026\000\003\000\025\000\004\000\022\000\000\000\
\\002\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 31
val numrules = 17
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | STRING of unit ->  (string)
 | BOOL of unit ->  (bool) | CHAR of unit ->  (char)
 | INT of unit ->  (int) | EXPLIST of unit ->  (sexp list)
 | EXPSTAR of unit ->  (sexp) | EXP of unit ->  (sexp)
 | START of unit ->  (sexp)
end
type svalue = MlyValue.svalue
type result = sexp
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 11) => true | (T 5) => true | (T 6) => true | (T 7) => true | 
(T 8) => true | (T 1) => true | (T 2) => true | (T 3) => true | (T 4)
 => true | _ => false
val preferred_change = 
(nil
,(T 4) :: nil
)::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "DOT"
  | (T 2) => "LPAREN"
  | (T 3) => "LVEC"
  | (T 4) => "RPAREN"
  | (T 5) => "QUOTESCHEME"
  | (T 6) => "QUASIQUOTE"
  | (T 7) => "UNQUOTE"
  | (T 8) => "UNQUOTESPLICING"
  | (T 9) => "INT"
  | (T 10) => "CHAR"
  | (T 11) => "BOOL"
  | (T 12) => "STRING"
  | (T 13) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 13) => MlyValue.ID(fn () => ("*bogus*")) | 
_ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.EXP EXP1,EXP1left,EXP1right))::rest671) => let val 
result=MlyValue.START(fn _ => let val EXP as EXP1=EXP1 ()
 in (EXP) end
)
 in (LrTable.NT 0,(result,EXP1left,EXP1right),rest671) end
| (1,(_,(MlyValue.INT INT1,INT1left,INT1right))::rest671) => let val 
result=MlyValue.EXP(fn _ => let val INT as INT1=INT1 ()
 in (INTsexp INT) end
)
 in (LrTable.NT 1,(result,INT1left,INT1right),rest671) end
| (2,(_,(MlyValue.CHAR CHAR1,CHAR1left,CHAR1right))::rest671) => let 
val result=MlyValue.EXP(fn _ => let val CHAR as CHAR1=CHAR1 ()
 in (CHARsexp CHAR) end
)
 in (LrTable.NT 1,(result,CHAR1left,CHAR1right),rest671) end
| (3,(_,(MlyValue.BOOL BOOL1,BOOL1left,BOOL1right))::rest671) => let 
val result=MlyValue.EXP(fn _ => let val BOOL as BOOL1=BOOL1 ()
 in (BOOLsexp BOOL) end
)
 in (LrTable.NT 1,(result,BOOL1left,BOOL1right),rest671) end
| (4,(_,(MlyValue.STRING STRING1,STRING1left,STRING1right))::rest671)
 => let val result=MlyValue.EXP(fn _ => let val STRING as STRING1=
STRING1 ()
 in (STRINGsexp STRING) end
)
 in (LrTable.NT 1,(result,STRING1left,STRING1right),rest671) end
| (5,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.EXP(fn _ => let val ID as ID1=ID1 ()
 in (SYMsexp ID) end
)
 in (LrTable.NT 1,(result,ID1left,ID1right),rest671) end
| (6,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXPSTAR EXPSTAR1,_,_))::(_,(
_,LPAREN1left,_))::rest671) => let val result=MlyValue.EXP(fn _ => 
let val EXPSTAR as EXPSTAR1=EXPSTAR1 ()
 in (EXPSTAR) end
)
 in (LrTable.NT 1,(result,LPAREN1left,RPAREN1right),rest671) end
| (7,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXP EXP2,_,_))::_::(_,(
MlyValue.EXPLIST EXPLIST1,_,_))::(_,(MlyValue.EXP EXP1,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.EXP(fn _ => let 
val EXP1=EXP1 ()
val EXPLIST as EXPLIST1=EXPLIST1 ()
val EXP2=EXP2 ()
 in (dottedPair (EXP1::EXPLIST) EXP2) end
)
 in (LrTable.NT 1,(result,LPAREN1left,RPAREN1right),rest671) end
| (8,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXPLIST EXPLIST1,_,_))::(_,(
_,LVEC1left,_))::rest671) => let val result=MlyValue.EXP(fn _ => let 
val EXPLIST as EXPLIST1=EXPLIST1 ()
 in (VECsexp (Vector.fromList EXPLIST)) end
)
 in (LrTable.NT 1,(result,LVEC1left,RPAREN1right),rest671) end
| (9,(_,(MlyValue.EXP EXP1,_,EXP1right))::(_,(_,QUOTESCHEME1left,_))::
rest671) => let val result=MlyValue.EXP(fn _ => let val EXP as EXP1=
EXP1 ()
 in (abbrevPrefix "quote" EXP) end
)
 in (LrTable.NT 1,(result,QUOTESCHEME1left,EXP1right),rest671) end
| (10,(_,(MlyValue.EXP EXP1,_,EXP1right))::(_,(_,QUASIQUOTE1left,_))::
rest671) => let val result=MlyValue.EXP(fn _ => let val EXP as EXP1=
EXP1 ()
 in (abbrevPrefix "quasiquote" EXP) end
)
 in (LrTable.NT 1,(result,QUASIQUOTE1left,EXP1right),rest671) end
| (11,(_,(MlyValue.EXP EXP1,_,EXP1right))::(_,(_,UNQUOTE1left,_))::
rest671) => let val result=MlyValue.EXP(fn _ => let val EXP as EXP1=
EXP1 ()
 in (abbrevPrefix "unquote" EXP) end
)
 in (LrTable.NT 1,(result,UNQUOTE1left,EXP1right),rest671) end
| (12,(_,(MlyValue.EXP EXP1,_,EXP1right))::(_,(_,UNQUOTESPLICING1left,
_))::rest671) => let val result=MlyValue.EXP(fn _ => let val EXP as 
EXP1=EXP1 ()
 in (abbrevPrefix "unquote-splicing" EXP) end
)
 in (LrTable.NT 1,(result,UNQUOTESPLICING1left,EXP1right),rest671) end
| (13,rest671) => let val result=MlyValue.EXPSTAR(fn _ => (NILsexp))
 in (LrTable.NT 2,(result,defaultPos,defaultPos),rest671) end
| (14,(_,(MlyValue.EXPSTAR EXPSTAR1,_,EXPSTAR1right))::(_,(
MlyValue.EXP EXP1,EXP1left,_))::rest671) => let val result=
MlyValue.EXPSTAR(fn _ => let val EXP as EXP1=EXP1 ()
val EXPSTAR as EXPSTAR1=EXPSTAR1 ()
 in (CONSsexp (EXP, EXPSTAR)) end
)
 in (LrTable.NT 2,(result,EXP1left,EXPSTAR1right),rest671) end
| (15,rest671) => let val result=MlyValue.EXPLIST(fn _ => ([]))
 in (LrTable.NT 3,(result,defaultPos,defaultPos),rest671) end
| (16,(_,(MlyValue.EXPLIST EXPLIST1,_,EXPLIST1right))::(_,(
MlyValue.EXP EXP1,EXP1left,_))::rest671) => let val result=
MlyValue.EXPLIST(fn _ => let val EXP as EXP1=EXP1 ()
val EXPLIST as EXPLIST1=EXPLIST1 ()
 in (EXP::EXPLIST) end
)
 in (LrTable.NT 3,(result,EXP1left,EXPLIST1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Scheme_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun LVEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun QUOTESCHEME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun QUASIQUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun UNQUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun UNQUOTESPLICING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun CHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.CHAR (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
