
object HW3 extends HW3Provided {

  /**
  1. Write a function only_capitals that takes a string list and returns a string list that has only the strings in
  the argument that start with an uppercase letter. Assume all strings have at least 1 character. Use List.filter,
  Char.isUpper, and String.sub to make a 1-2 line solution.
    */
  def onlyCapitals(strings: Seq[String]): Seq[String] = strings.filter(_.head.isUpper)

  /**
  2. Write a function longest_string1 that takes a string list and returns the longest string in the list. If the list
  is empty, return "". In the case of a tie, return the string closest to the beginning of the list. Use foldl,
  String.size, and no recursion (other than the implementation of foldl is recursive).
    */
  def longestString1(strings: Seq[String]): String =
    strings.foldLeft("")((res, str) =>
      if (str.size > res.size) str
      else res)
}

/**
Provided originally from hw3provided.sml
  */
class HW3Provided {

  class NoAnswer extends Exception

  sealed abstract class Pattern
  case object Wildcard extends Pattern
  case object UnitP extends Pattern
  case class Variable(value: String) extends Pattern
  case class ConsP(value: Int) extends Pattern
  case class TupleP(values: Seq[Pattern]) extends Pattern
  case class ConstructorP(name: String, value: Pattern) extends Pattern

  sealed abstract class Valu
  case object Unit extends Valu
  case class Const(value: Int) extends Valu
  case class Tuple(values: Seq[Valu]) extends Valu
  case class Constructor(name: String, value: Valu) extends Valu

  def g(f1: () => Int, f2: (String) => Int, p: Pattern): Int = {
    val r = g(f1, f2, _:Pattern)
    p match {
      case Wildcard => f1()
      case Variable(x) => f2(x)
      case TupleP(ps) => ps.foldLeft(0)((res, pattern) => r(pattern) + res)
      case ConstructorP(_, pt) => r(pt)
      case _ => 0
    }
  }

  // for the challenge problem only
  sealed abstract class Typ
  case object Anything extends Typ
  case object UnitT extends Typ
  case object IntT extends Typ
  case class TupleT(values: Seq[Typ]) extends Typ
  case class Datatype(value: String) extends Typ
}
