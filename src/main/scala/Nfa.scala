import java.util.regex.Pattern
import scala.annotation.tailrec

class Nfa[A](name : String,states : List[(A, List[(Char,A)],Boolean)],start : A) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] =
  {
    val states = this.states.map(x => (f(x._1),x._2.map(y =>(y._1,f(y._2))),x._3))
    new Nfa[B]("copy" + name,states, f(start))
  }

  def next(state:A, c: Char): Set[A] =
  {
    val x = states find{x => x._1 == state}
    val y = x.head._2.foldRight(Set[A]())((y,acc) => if(y._1 == c) acc + y._2 else acc)
    @tailrec
    def epsClose(set: Set[A],closed:Set[A]):Set[A] =
    {
      if (set.isEmpty)
      {
        closed
      }
      else
      {
        val state = set.head
        if (closed.contains(state))
        {
          epsClose(set.tail,closed)
        }
        else
        {
          val x = states find { x => x._1 == state }
          val y = x.head._2.foldRight(Set[A]())((y, acc) => if (y._1 == 'ε') acc + y._2 else acc)
          epsClose( set | y,closed + state)
        }
      }


    }
    epsClose(y,Set[A]())
    //y.foldRight(y)((x,acc) => acc ++ next(x,'ε'))
  }

  def accepts(str: String): Boolean =
  {
    def accepts_state(string : String,state : A): Boolean =
    {
      val x = states find{x => x._1 == state}
      if (string == "" && x.head._3)
      {
        true
      }
      else if(string != "")
      {
        val aux = x.head._2.filter(x => x._1 == string.head)
        val aux2 = x.head._2.filter(x => x._1 == 'ε')
        val char = aux.foldRight(false)((x, acc) => acc || accepts_state(string.tail, x._2))
        val esp = aux2.foldRight(false)((x, acc) => acc || accepts_state(string, x._2))
        char || esp
      }
      else
      {
        val aux = x.head._2.filter(x => x._1 == 'ε')
        aux.foldRight(false)((x, acc) => acc || accepts_state(string, x._2))
      }
    }
    accepts_state(str,start)
  }

  def getStates : Set[A] =
  {
    states.foldRight(Set[A]())((x,acc)=> acc + x._1)
  }

  def isFinal(state: A): Boolean =
  {
    val x = states find{x => x._1 == state}
    x.head._3
  }

  def allFalse(): Nfa[A] =
  {
    new Nfa[A]("copy",states.map(((x : A,y: List[(Char,A)],_ : Boolean) => (x,y,false)).tupled),start)
  }

  def getConfiguration: List[(A, List[(Char,A)],Boolean)] =
  {
    states
  }

  def getStart : A =
  {
    start
  }

  def AddTranzition (state1 : A, state2 : A, char : Char ) : Nfa[A] =
  {
    def add(aux_states: List[(A, List[(Char, A)], Boolean)]): List[(A, List[(Char, A)], Boolean)] =
    {
      aux_states match {
        case Nil => Nil
        case (y, x, z) :: _ =>
          if (y == state1) {
            (y,x ++ List((char,state2)),z) :: aux_states.tail
          }
          else {
            (y,x,z) :: add(aux_states.tail)
          }
      }
    }
    new Nfa("Nfa",add(states),start)
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def fromPrenex(str: String): Nfa[Int] =
  {
    val (nfa, _) = str match
    {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case "Void" => (VoidNfa.allFalse(),"")
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case x => Simple(x)
    }

    nfa
  }

  def Union(str :String) :(Nfa[Int],String) =
  {
    val (nfa1 , str2) = str match
    {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$x$_" => Simple(str)
    }

    val (nfa2, str3) = str2 match
    {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$_$_" => Simple(str2)
    }
    // nfa1 U nfa2
    val size1 = nfa1.getStates.size
    val size2 = nfa2.getStates.size
    val start = VoidNfa.allFalse()
    val end = VoidNfa.map(x => x + size1 + size2 + 1)
    val nfa1fin = nfa1.map(x => x + 1).allFalse().AddTranzition(size1,end.getStart,'ε')
    val nfa2fin = nfa2.map(x => x + 1 + size1).allFalse().AddTranzition(size1 + size2,end.getStart,'ε')

    val states = start.AddTranzition(0,nfa1fin.getStart,'ε').AddTranzition(0,nfa2fin.getStart,'ε').getConfiguration ++ nfa1fin.getConfiguration ++ nfa2fin.getConfiguration ++ end.getConfiguration

    (new Nfa("UNfa",states,0),str3)
  }

  def Concat(str: String): (Nfa[Int],String) =
  {
    val (nfa1, str2) = str match {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$_$_" => Simple(str)
    }

    val (nfa2, str3) = str2 match {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$_$_" => Simple(str2)
    }
    // nfa1nfa2
    val size = nfa1.getStates.size
    val states = nfa1.allFalse().AddTranzition(size - 1, nfa2.getStart + size, 'ε').getConfiguration ++ nfa2.map(x => x + size).getConfiguration

    (new Nfa("CATNfa", states, nfa1.getStart), str3)
  }

  def Star(str: String): (Nfa[Int],String) =
  {
    val (nfa , str2) =str match
    {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$_$_" => Simple(str)
    }

    //nfa*
    val size = nfa.getStates.size
    val start = VoidNfa.AddTranzition(0,size + 1,'ε').AddTranzition(0,1,'ε')
    val end = VoidNfa.map(x => x + size + 1)

    val states = start.allFalse().getConfiguration ++ nfa.AddTranzition(size - 1,0,'ε').map(x=> x + 1).AddTranzition(size,size+1,'ε').allFalse().getConfiguration ++ end.getConfiguration

    (new Nfa("Nfa*", states, 0),str2)
  }

  def Plus(str: String): (Nfa[Int],String) =
  {
    val (nfa, str2) = str match {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$x$_" => Simple(str)
    }

    //nfa+
    val size = nfa.getStates.size
    val start = VoidNfa.AddTranzition(0, size + 1, 'ε').AddTranzition(0, 1, 'ε')
    val end = VoidNfa.map(x => x + size + 1)

    val states = nfa.AddTranzition(size -1,size,'ε').getConfiguration ++ start.allFalse().map(x => x + size).getConfiguration ++ nfa.AddTranzition(size - 1, 0, 'ε').map(x => x + 1 + size).AddTranzition(2 * size, 2 * size + 1, 'ε').allFalse().getConfiguration ++ end.map(x=> x + size).getConfiguration

    (new Nfa("Nfa+", states, 0), str2)
  }

  def Maybe(str: String): (Nfa[Int],String) =
  {
    val (nfa, str2) = str match {
      case s"UNION $xs" => Union(xs)
      case s"CONCAT $xs" => Concat(xs)
      case s"STAR $xs" => Star(xs)
      case s"PLUS $xs" => Plus(xs)
      case s"MAYBE $xs" => Maybe(xs)
      case s"eps$xs" => Simple("ε" ++ xs)
      case s"' '$xs" => Simple(" " ++ xs)
      case s"$x$_" => Simple(str)
    }

    //nfa+
    val size = nfa.getStates.size
    val start = VoidNfa.AddTranzition(0, size + 1, 'ε').AddTranzition(0, 1, 'ε')
    val end = VoidNfa.map(x => x + size + 1)

    val states = start.allFalse().getConfiguration ++  nfa.map(x => x + 1).AddTranzition(size,size+1,'ε').allFalse().getConfiguration ++ end.getConfiguration

    (new Nfa("Nfa+", states, 0), str2)
  }

  def Simple(str: String): (Nfa[Int],String) =
  {
    val states = List((0,List((str.head,1)),false),(1,List(),true))
    (new Nfa[Int]("NFA",states,0),str.tail.tail)
  }

  def VoidNfa : Nfa[Int] =
  {
    val states = List((0,List(),true))
    new Nfa[Int]("Void",states,0)
  }
  // You can add more methods to this object
}