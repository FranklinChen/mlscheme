signature Scheme_TOKENS =
sig
type ('a,'b) token
type svalue
val ID: (string) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val BOOL: (bool) *  'a * 'a -> (svalue,'a) token
val CHAR: (char) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val UNQUOTESPLICING:  'a * 'a -> (svalue,'a) token
val UNQUOTE:  'a * 'a -> (svalue,'a) token
val QUASIQUOTE:  'a * 'a -> (svalue,'a) token
val QUOTESCHEME:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LVEC:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Scheme_LRVALS=
sig
structure Tokens : Scheme_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
