import java.lang
import scala.annotation.tailrec

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] =
  {
    s match
    {
      case '\'':: 'P'::'A'::'R'::'\''::xs => Left('(') :: preprocess(xs)
      case '\'':: 'C'::'P'::'A'::'R'::'\''::xs => Left(')') :: preprocess(xs)
      case 'e'::'p'::'s'::xs => Left('ε') :: preprocess(xs)
      case Nil => List()
      case '['::a::'-'::b::']'::xs => List(Right('(')) ++ atob(a,b)++ List(Right(')')) ++ preprocess(xs)
      case '\''::x::'\''::xs => List(Left(x)) ++ preprocess(xs)
      case '\''::'\\'::'n'::'\''::xs =>List(Left('\n')) ++ preprocess(xs)
      case '\''::'\\'::'t'::'\''::xs =>List(Left('\t')) ++ preprocess(xs)
      case x::xs =>
        if("*|+?()" contains x)
        {
          List(Right(x))++ preprocess(xs)
        }
        else
        {
          List(Left(x)) ++ preprocess(xs)
        }
    }
  }

  def atob(a: Char,b:Char):List[Either[Char,Char]]=
  {
    if (a == b)
      {
        return List(Left(b));
      }
    List(Left(a),Right('|')) ++ atob((a + 1).toChar, b)
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String =
  {
    val preproc = preprocess(addParantetheses(str).toList)
    def parseRegex(p:List[Either[Char,Char]],stack:String): String =
    {
        p match
        {
          case Left(x)::xs =>
            if (stack == "")
              {
                parseRegex(xs,s"$x" )
              }
              else
              {
                val a = parseRegex(xs,s"$x")
                s"CONCAT $stack $a"
              }
          case Right('(')::xs => val (x,y) = splitParantheses(xs,List(),1)
            val s1 = parseRegex(x,"")
            if (stack == "")
            {
              parseRegex(y, s1)
            }
            else
            {
              val aux = parseRegex(y, s1)
              s"CONCAT $stack " ++ aux
            }
          case Right('|')::xs =>
            {
              val a = parseRegex(xs,"")
              s"UNION $stack $a"
            }
          case Right('*')::xs => parseRegex(xs,s"STAR $stack" )
          case Right('?')::xs => parseRegex(xs,s"MAYBE $stack" )
          case Right('+')::xs => parseRegex(xs,s"PLUS $stack" )
          case Nil => stack
        }
    }
    parseRegex(preproc,"")
  }

  @tailrec
  //splits (a)b into a b, if there are other () or multple () in each other ex: (()) it takes the first and the largest
  def splitParantheses(l:List[Either[Char,Char]], stack:List[Either[Char,Char]], nr:Int): (List[Either[Char,Char]],List[Either[Char,Char]]) =
  {
    l match {
      case Right(')') ::xs =>
        if ( nr == 1)
          {
            (stack,xs)
          }
        else
          {
            splitParantheses(xs,stack ++ List(Right(')')),nr -1)
          }
      case Right('(') ::xs => splitParantheses(xs,stack ++ List(Right('(')),nr +1)
      case Left(x)::xs => splitParantheses(xs,stack ++ List(Left(x)),nr)
      case Right(x)::xs => splitParantheses(xs,stack ++ List(Right(x)),nr)
    }
  }

  //add parantheses to the text as to exemplify operation order
  def addParantetheses(string: String): String =
  {
    string match
    {
      case s"|$c" =>
      {
        val aux = addParantetheses(c)
        s"|($aux)"
      }
      case s"($_)*$_" =>
      {
        val (c, b, a) = splitter("", string, "", 0)
        val x = addParantetheses(a)
        val y = addParantetheses(b.tail)
        val z = addParantetheses(c)
        if(b == "")
        {
          s"$z(($x)*)$y"
        }
        else
        {
          val op = b.head
          if (op == '|')
          {
            s"$z($x)$op$y"
          }
          else
          {
            s"$z(($x)$op)$y"
          }

        }
      }
      case s"($_)+$_" =>
      {
        val (c, b, a) = splitter("", string, "", 0)
        val x = addParantetheses(a)
        val y = addParantetheses(b.tail)
        val z = addParantetheses(c)
        if (b == "") {
          s"$z(($x)+)$y"
        }
        else {
          val op = b.head
          if (op == '|')
          {
            s"$z($x)$op$y"
          }
          else
          {
            s"$z(($x)$op)$y"
          }
        }
      }
      case s"$_($_)$_" =>
      {
        val (c,b,a) = splitter("",string,"",0)
        val x = addParantetheses(a)
        val y = addParantetheses(b)
        val z = addParantetheses(c)
        s"$z($x)$y"
      }
      case s"$a|$b" =>
      {
        val aux = addParantetheses(b)
        s"($a)|($aux)"
      }
      case x => x
    }
  }
  //splits c(b)a into c b a, if there are other () or multple () in each other ex: (()) it takes the first and the largest
  @tailrec
  def splitter(string1: String, string2:String, string3:String, nr:Int):(String,String,String) =
  {
      val char = string2.head
      if (nr == 0)
      {
        if (string2.head == '(')
        {
          splitter(string1,string2.tail,string3,nr + 1)
        }
        else
        {
          splitter(string1 ++ s"$char",string2.tail,string3,nr)
        }
      }
      else
      {
        if (string2.head == '(')
        {
          splitter(string1 , string2.tail, string3 ++ s"$char", nr + 1)
        }
        else if (string2.head == ')')
        {
          if (nr == 1)
          {
            (string1,string2.tail,string3)
          }
          else
          {
            splitter(string1 , string2.tail, string3 ++ s"$char", nr - 1)
          }

        }
        else {
          splitter(string1 , string2.tail, string3 ++ s"$char", nr)
        }
      }
  }

}
