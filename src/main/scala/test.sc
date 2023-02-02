import scala.annotation.tailrec

val spec =
  """#BEGIN: (' '|'\n'|'\t')*begin(' '|'\n'|'\t')*;
    #END: (' '|'\n'|'\t')*end(' '|'\n'|'\t')*;
    #EQUAL: (' '|'\n'|'\t')*==(' '|'\n'|'\t')*;
    #ASSIGN: (' '|'\n'|'\t')*=(' '|'\n'|'\t')*;
    #PLUS: (' '|'\n'|'\t')*'+'(' '|'\n'|'\t')*;
    #MINUS: (' '|'\n'|'\t')*'-'(' '|'\n'|'\t')*;
    #MULTIPLY: (' '|'\n'|'\t')*'*'(' '|'\n'|'\t')*;
    #GREATER: (' '|'\n'|'\t')*>(' '|'\n'|'\t')*;
    #WHILE: (' '|'\n'|'\t')*while(' '|'\n'|'\t')*;
    #DO: (' '|'\n'|'\t')*dp(' '|'\n'|'\t')*;
    #OD: (' '|'\n'|'\t')*od(' '|'\n'|'\t')*;
    #IF: (' '|'\n'|'\t')*if(' '|'\n'|'\t')*;
    #THEN: (' '|'\n'|'\t')*then(' '|'\n'|'\t')*;
    #ELSE: (' '|'\n'|'\t')*else(' '|'\n'|'\t')*;
    #FI: (' '|'\n'|'\t')*fi(' '|'\n'|'\t')*;
    #RETURN: (' '|'\n'|'\t')*return(' '|'\n'|'\t')*;
    #OPEN_PARANTHESIS: (' '|'\n'|'\t')*'PAR'(' '|'\n'|'\t')*;
    #CLOSE_PARANTHESIS: (' '|'\n'|'\t')*'CPAR'(' '|'\n'|'\t')*;
    #NUMBER: (' '|'\n'|'\t')*0|([1-9][0-9]*)(' '|'\n'|'\t')*;
    #VARIABLE: (' '|'\n'|'\t')*[a-z]*(' '|'\n'|'\t')*;
    #""".stripMargin('#')

Dfa.fromPrenex(Regex.toPrenex("(' '|'\n'|'\t')*'CPAR'(' '|'\n'|'\t')*")).accepts("(")
Regex.toPrenex("(' '|'\n'|'\t')*'CPAR'(' '|'\n'|'\t')*")
val (x,b) =Lexer(spec).parseSpec(spec)
val l = Lexer(spec).createNfas(b,x)
val start = Nfa.VoidNfa.allFalse().map(_ => "Start")
start.getConfiguration
val nfa = Lexer(spec).uniteNfas(l, start, List())
nfa.getConfiguration.size
val dfa  = Dfa.fromNfa(nfa)
dfa.accepts("(")
var x = Lexer(spec).lex("begin\na = 5\nwhile (a > 1) do\n    a = a - 1\nod\nend")
x match
{
  case Left(l) => l
  case Right(value) => value.map(x=> x._2)
}




