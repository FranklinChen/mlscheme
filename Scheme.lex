(* Note required interface with ML yacc *)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val pos = ref 0
fun eof () = Tokens.EOF (!pos, !pos)

(* Should declare exception for lexer *)
(* Lots of token stuff omitted; also, should process strings *)

%%

%header (functor SchemeLexFun (structure Tokens: Scheme_TOKENS));

ws = [\ \t];
nl = "\n";

letter = [A-Za-z];
digit = [0-9];

initial = {letter}|[!$%&*/:<=>?~_^];
subsequent = {initial}|{digit}|[.+-];
identifier = {initial}{subsequent}*|"+"|"-"|"...";

strchar = "\\\""|"\\\\"|[^\"\\];
str = \"{strchar}*\";

radix2 = "#b";
radix8 = "#o";
radix10 = "#d"?;
radix16 = "#x";

digit2 = [01];
digit8 = [0-7];
digit10 = {digit};
digit16 = {digit}|[a-f];

sign = [+-]?;
exponentmarker = [esfdl];
exponent = {exponentmarker}{sign}{digit10}+;
suffix = {exponent}?;
exactness = ("#i"|"#e")?;

%%

{nl} => (pos := (!pos) + 1; lex());
{ws}+ => (lex());
";".*{nl} => (pos := (!pos) + 1; lex());

"." => (Tokens.DOT (!pos, !pos));
"(" => (Tokens.LPAREN (!pos, !pos));
"#(" => (Tokens.LVEC (!pos, !pos));
")" => (Tokens.RPAREN (!pos, !pos));

"'" => (Tokens.QUOTESCHEME (!pos, !pos));
"`" => (Tokens.QUASIQUOTE (!pos, !pos));
"," => (Tokens.UNQUOTE (!pos, !pos));
",@" => (Tokens.UNQUOTESPLICING (!pos, !pos));

{digit}+ => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));

"#f" => (Tokens.BOOL (false, !pos, !pos));
"#t" => (Tokens.BOOL (true, !pos, !pos));

"#\\". => (Tokens.CHAR (String.sub (yytext, 2), !pos, !pos));
"#\\newline" => (Tokens.CHAR (#"\n", !pos, !pos));
"#\\space" => (Tokens.CHAR (#" ", !pos, !pos));

{str} => (Tokens.STRING (String.substring(yytext, 1, String.size(yytext) - 2),
                         !pos, !pos));

{identifier} => (Tokens.ID (yytext, !pos, !pos));
