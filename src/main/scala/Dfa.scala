import scala.annotation.tailrec

class Dfa[A] (name : String,states : List[(A, List[(Char,A)],Boolean)],start : A){


  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] =
  {
    val states = this.states.map(x => (f(x._1), x._2.map(y => (y._1, f(y._2))), x._3))
    new Dfa[B]("copy" + name, states, f(start))
  }

  def next(state:A, c: Char): A =
  {
    val x = states find { x => x._1 == state }
    if (state == states.last._1)
    {
      states.last._1
    }
    else
    {
      val list = x.head._2 find { y => y._1 == c }
      if (list.isEmpty)
      {
        states.last._1
      }
      else
      {
        list.head._2
      }
    }
  }

  def accepts(str: String): Boolean =
  {
    @tailrec
    def accepts_aux(string: String, state: A): Boolean =
    {
      val x = states find{x => x._1 == state}
      if (x.head._3 && string == "")
      {
          true
      }
      else if (string != "")
      {
        accepts_aux(string.tail,next(state,string.head))
      }
      else
      {
          false
      }
    }
    accepts_aux(str,this.start)
  }

  def getStates : Set[A] =
  {
    states.foldRight(Set[A]())((x, acc) => acc + x._1)
  }

  def isFinal(state: A): Boolean =
  {
    val x = states find { x => x._1 == state }
    x.head._3
  }

  def AddTranzition(state1: A, state2: A, char: Char): Dfa[A] =
  {
    def add(aux_states: List[(A, List[(Char, A)], Boolean)]): List[(A, List[(Char, A)], Boolean)] =
    {
      aux_states match
      {
        case Nil => Nil
        case (y, x, z) :: _ =>
          if (y == state1)
          {
            (y, x ++ List((char, state2)), z) :: aux_states.tail
          }
          else
          {
            (y, x, z) :: add(aux_states.tail)
          }
      }
    }
    new Dfa("Dfa", add(states), start)
  }

  def getConfiguration: List[(A, List[(Char, A)], Boolean)] =
  {
    states
  }

  def acceptslongest(str: String):(String,A,String) =
  {
    @tailrec
    def accepts_aux(string: String, state: A,state2: A, tailstr: String,stack : String):(String,A,String)  =
    {
      val x = states find { x => x._1 == state }
      if (x.head._3 && string == "")
      {
        ("",x.head._1,stack)
      }
      else if(x.head._3 && string != "")
      {
        accepts_aux(string.tail,next(state,string.head),state,string,stack + string.head )
      }
      else if(string != "")
      {
          accepts_aux(string.tail,next(state,string.head),state2,tailstr,stack + string.head)
      }
      else
      {
        (tailstr,state2,str.take(str.length - tailstr.length))
      }
    }

    accepts_aux(str, this.start,this.start,str,"")
  }

  def returnAtSink(str: String): Int =
  {
    @tailrec
    def accepts_aux(string: String, state: A): Int =
    {
      val x = states find { x => x._1 == state }
      if (state == states.last._1)
      {
          str.length - string.length
      }
      else if (string == "")
      {
          0
      }
      else
      {
        accepts_aux(string.tail, next(state, string.head))
      }
    }
    accepts_aux(str, this.start)
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] =
  {
    val nfa = Nfa.fromPrenex(str)
    val alphabet = get_alphabet(nfa)
    val start = nfa.next(nfa.getStart,'ε') + nfa.getStart
    val sink = create_Sink(alphabet)

    @tailrec
    def Nfa_to_Dfa(nfa : Nfa[Int],start: List[Int],alphabet : Set[Char],abc : List[Char],state : (List[Int], List[(Char,List[Int])],Boolean), allstates :Set[Set[Int]],stack : List[(List[Int], List[(Char,List[Int])],Boolean)],config: List[(List[Int], List[(Char,List[Int])],Boolean)]): List[(List[Int], List[(Char,List[Int])],Boolean)] =
    {
      abc match
      {
        case Nil =>
          if(stack.isEmpty)
          {
              config ++ List(state)
          }
          else
          {
            val next = stack.head
            Nfa_to_Dfa(nfa,next._1 , alphabet, alphabet.toList, next,allstates,stack.tail,config ++ List(state))
          }

        case x::xs =>
          val nextStates = start.foldRight(Set[Int]())((y,acc)=> acc ++ nfa.next(y,x))
          if (nextStates.isEmpty)
            {
              val aux_state = (state._1,state._2 ++ List((x,List(-1))),state._3)
              Nfa_to_Dfa(nfa, start, alphabet, xs, aux_state,allstates,stack,config)
            }
          else if(! allstates.contains(nextStates))
            {
              val states = nextStates.toList.sorted
              val aux_state = (state._1, state._2 ++ List((x,states)),state._3)
              val aux_next_state = (states, List(),isFinal(nextStates,nfa))
              val newallstates = allstates + nextStates
              Nfa_to_Dfa(nfa, start, alphabet, xs, aux_state,newallstates,stack ++ List(aux_next_state),config)
            }
          else
            {
              val states = nextStates.toList.sorted
              val aux_state = (state._1, state._2 ++ List((x, states)), state._3)
              Nfa_to_Dfa(nfa, start, alphabet, xs, aux_state, allstates,stack,config)
            }
      }
    }
    val states = Nfa_to_Dfa(nfa,start.toList,alphabet,alphabet.toList,(start.toList,List(),isFinal(start,nfa)),Set(start),List(),List()) ++ sink.map(x=> List(x)).getConfiguration
    val dfa = new Dfa[List[Int]]("Dfa-List",states,start.toList)
    val newStates = dfa.getStates
    dfa.map(x=> newStates.toList.indexOf(x))
  }

  def isFinal(states : Set[Int] , nfa :Nfa[Int]) : Boolean =
  {
    states.foldRight(false)((x,acc) => acc || nfa.isFinal(x))
  }

  def get_alphabet (nfa : Nfa[Int]) : Set[Char] =
  {
      val x = nfa.getConfiguration
      x.foldRight(Set[Char]())((x,acc) => acc ++ x._2.foldRight(Set[Char]())((x,acc) => acc + x._1)) - 'ε'
  }

  def create_Sink(alphabet: Set[Char]): Dfa[Int] =
  {
    val states = List((-1, List(), false))
    val dfa = new Dfa[Int]("Sink", states, -1)

    @tailrec
    def AddtoSelf(abcs : List[Char], dfa: Dfa[Int]): Dfa[Int] =
    {
      abcs match
      {
        case Nil => dfa
        case x :: xs => AddtoSelf(xs, dfa.AddTranzition(-1, -1, x))
      }
    }
    AddtoSelf(alphabet.toList,dfa)
  }


  //same as above but for string
  def fromNfa(nfa: Nfa[String]): Dfa[List[String]] = {
    val alphabet = get_alphabet2(nfa)
    val start = nfa.next(nfa.getStart, 'ε') + nfa.getStart
    val sink = create_Sink2(alphabet)

    @tailrec
    def Nfa_to_Dfa(nfa: Nfa[String], start: List[String], alphabet: Set[Char], abc: List[Char], state: (List[String], List[(Char, List[String])], Boolean), allstates: Set[Set[String]], stack: List[(List[String], List[(Char, List[String])], Boolean)], config: List[(List[String], List[(Char, List[String])], Boolean)]): List[(List[String], List[(Char, List[String])], Boolean)] = {
      abc match {
        case Nil =>
          if (stack.isEmpty) {
            config ++ List(state)
          }
          else {
            val next = stack.head
            Nfa_to_Dfa(nfa, next._1, alphabet, alphabet.toList, next, allstates, stack.tail, config ++ List(state))
          }

        case x :: xs =>
          val nextStates = start.foldRight(Set[String]())((y, acc) => acc ++ nfa.next(y, x))
          if (nextStates.isEmpty) {
            val aux_state = (state._1, state._2 ++ List((x, List("Sink"))), state._3)
            Nfa_to_Dfa(nfa, start, alphabet, xs, aux_state, allstates, stack, config)
          }
          else if (!allstates.contains(nextStates)) {
            val states = nextStates.toList.sorted
            val aux_state = (state._1, state._2 ++ List((x, states)), state._3)
            val aux_next_state = (states, List(), isFinal2(nextStates, nfa))
            val newallstates = allstates + nextStates
            Nfa_to_Dfa(nfa, start, alphabet, xs, aux_state, newallstates, stack ++ List(aux_next_state), config)
          }
          else {
            val states = nextStates.toList.sorted
            val aux_state = (state._1, state._2 ++ List((x, states)), state._3)
            Nfa_to_Dfa(nfa, start, alphabet, xs, aux_state, allstates, stack, config)
          }
      }
    }

    val states = Nfa_to_Dfa(nfa, start.toList, alphabet, alphabet.toList, (start.toList, List(), isFinal2(start, nfa)), Set(start), List(), List()) ++ sink.map(x => List(x)).getConfiguration
    new Dfa[List[String]]("Dfa", states, start.toList)
  }

  def get_alphabet2(nfa: Nfa[String]): Set[Char] = {
    val x = nfa.getConfiguration
    x.foldRight(Set[Char]())((x, acc) => acc ++ x._2.foldRight(Set[Char]())((x, acc) => acc + x._1)) - 'ε'
  }

  def create_Sink2(alphabet: Set[Char]): Dfa[String] = {
    val states = List(("Sink", List(), false))
    val dfa = new Dfa[String]("Sink", states, "Sink")

    @tailrec
    def AddtoSelf(abcs: List[Char], dfa: Dfa[String]): Dfa[String] = {
      abcs match {
        case Nil => dfa
        case x :: xs => AddtoSelf(xs, dfa.AddTranzition("Sink", "Sink", x))
      }
    }

    AddtoSelf(alphabet.toList, dfa)
  }

  def isFinal2(states: Set[String], nfa: Nfa[String]): Boolean = {
    states.foldRight(false)((x, acc) => acc || nfa.isFinal(x))
  }
  // You can add more methods to this object
}
