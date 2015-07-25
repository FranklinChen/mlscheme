structure Join =
struct

structure SchemeLrVals = SchemeLrValsFun(structure Token = LrParser.Token)
structure SchemeLex = SchemeLexFun(structure Tokens = SchemeLrVals.Tokens)
structure SchemeParser = Join(structure Lex= SchemeLex
		              structure LrParser = LrParser
		              structure ParserData = SchemeLrVals.ParserData)

val parse = fn s =>
  let val dev = TextIO.openIn s
      val stream = SchemeParser.makeLexer(fn i => TextIO.inputN(dev,i))
      val _ = SchemeLex.UserDeclarations.pos := 1
      val error = fn (e,i:int,_) => TextIO.output(TextIO.stdErr,s ^ "," ^
		  " line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
  in SchemeParser.parse(30,stream,error,()) before TextIO.closeIn dev
  end

val keybd = fn () =>
  let val dev = TextIO.stdIn
      val stream = SchemeParser.makeLexer (fn i => Option.valOf(TextIO.inputLine dev))
      val _ = SchemeLex.UserDeclarations.pos := 1
      val error = fn (e,i:int,_) => TextIO.output(TextIO.stdErr,"stdIn," ^
		  " line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
  in SchemeParser.parse(0,stream,error,())
  end

val parseString = fn s =>
  let
      val source =
	  let
	      val first = ref true
	  in
	      fn (i:int) => if !first then (first := false; s) else ""
	  end
      val stream = SchemeParser.makeLexer source
      val _ = SchemeLex.UserDeclarations.pos := 1
      val error = fn (e,i:int,_) => TextIO.output(TextIO.stdErr,
		  "line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
  in SchemeParser.parse(0,stream,error,())
  end

end
