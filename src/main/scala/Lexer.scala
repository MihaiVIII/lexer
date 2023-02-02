import scala.annotation.tailrec

case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] =
  {
    val (tokens, regexes) = parseSpec(spec)
    val nfas = createNfas(regexes, tokens)
    val start = Nfa.VoidNfa.allFalse().map(_ => "Start")
    val nfa = uniteNfas(nfas, start, List())
    val dfa = Dfa.fromNfa(nfa)
    val biggerS = word
    def parseword(str: String,line : Int,char : Int): List[(String, String)] =
    {
      val (tailStr, states, word) = dfa.acceptslongest(str)
      val token = tokentype(states, tokens)
      if (tailStr.nonEmpty && tailStr != str)
      {
        val ltail = parseword(tailStr,biggerS.count(_ == '\n') - tailStr.count(_ == '\n'),char + word.length)
        if (ltail.head._2 == "ERROR")
        {
          ltail
        }
        else
        {
          (word, token) :: ltail
        }
      }
      else if(word.nonEmpty)
      {
        List((word,token))
      }
      else if(tailStr == str)
      {
        val chr = dfa.returnAtSink(str)
        if (chr == 0)
        {
          val erorMsg = s"No viable alternative at character EOF, line $line"
          List((erorMsg, "ERROR"))
        }
        else
        {
          val erorMsg = s"No viable alternative at character ${getIndex(biggerS,char,0) + chr-1}, line $line"
          List((erorMsg, "ERROR"))
        }

      }
      else
      {
          List()
      }
    }
    val out = parseword(word,0,0)
    if (out.head._2 == "ERROR")
    {
      Left(out.head._1)
    }
    else
    {
      Right(out)
    }
  }

  def parseSpec(str: String): (List[String], List[String]) =
  {
    str match
    {
      case s"$x: $y;$xs" => val (a, b) = parseSpec(xs.tail)
        (x :: a, y :: b)
      case s"" => (List(), List())
    }
  }

  def createNfas(regexes: List[String], tokens: List[String]): List[Nfa[String]] =
  {
    regexes match
    {
      case Nil => List()
      case x :: xs =>
        val pnx = Regex.toPrenex(x)
        Nfa.fromPrenex(pnx).map(x => tokens.head + x) :: createNfas(xs, tokens.tail)
    }
  }

  def uniteNfas(Nfas: List[Nfa[String]], start: Nfa[String], states: List[(String, List[(Char, String)], Boolean)]): Nfa[String] =
  {
    Nfas match
    {
      case x :: xs => val size = x.getStates.size
        val newstart = start.AddTranzition("Start", x.getStart, 'ε')
        val newstates = states ++ x.getConfiguration
        uniteNfas(xs, newstart, newstates)
      case Nil => new Nfa("Lexer", start.getConfiguration ++ states, "Start")
    }
  }

  def tokentype(states: List[String], tokens: List[String]): String =
  {
    @tailrec
    def parseList(string: String, list: List[String]): String =
    {
      list match
      {
        case Nil => ""
        case x :: xs => if (x.contains(string))
        {
          string
        }
        else
        {
          parseList(string, xs)
        }
      }
    }

    val token = parseList(tokens.head, states)
    if (token == "")
    {
      tokentype(states, tokens.tail)
    }
    else
    {
      token
    }
  }

  def getIndex(string: String,index:Int,result:Int):Int  =
  {
    if (string.isEmpty)
    {
      result
    }
    else if(string.take(index).isEmpty)
    {
        result
    }
    else if (string.take(index).last == '\n')
    {
      result
    }
    else
    {
      getIndex(string.init,index-1,result + 1)
    }
  }
}