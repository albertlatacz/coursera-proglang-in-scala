
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


  /**
  3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties it returns the
  string closest to the end of the list. Your solution should be almost an exact copy of longest_string1.
    */
  def longestString2(strings: Seq[String]): String =
    strings.foldLeft("")((res, str) =>
      if (str.size >= res.size) str
      else res)


  /**
  4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
  - longest_string3 has the same behavior as longest_string1 and longest_string4 has the same behavior as longest_string2.
  - longest_string_helper has type (int * int -> bool) -> string list -> string
    (notice the currying). This function will look a lot like longest_string1 and longest_string2 but is more general
     because it takes a function as an argument.
  - longest_string3 and longest_string4 are defined with val-bindings and partial applications of longest_string_helper.
    */
  private def longestStringHelper(sizeComparator : (Int, Int) => Boolean)(strings: Seq[String]): String =
    strings.foldLeft("")((res, str) =>
      if (sizeComparator(str.size, res.size)) str
      else res)

  def longestString3 = longestStringHelper(_ > _)(_)

  def longestString4 = longestStringHelper(_ >= _)(_)


  /**
  5. Write a function longest_capitalized that takes a string list and returns the longest string in the list that
  begins with an uppercase letter (or "" if there are no such strings). Use a val-binding and the ML library’s o
  operator for composing functions. Resolve ties like in problem 2.
    */
  def longestCapitalized(strings: Seq[String]): String = {
    val findLongestCapitalized =  (longestString1 _) compose (onlyCapitals _)
    findLongestCapitalized(strings)
  }


  /**
  6. Write a function rev_string that takes a string and returns the string that is the same characters in reverse
  order. Use ML’s o operator, the library function rev for reversing lists, and two library functions in the String
  module. (Browse the module documentation to find the most useful functions.)
    */
  def revString(str: String): String = str.reverse


  /**
  7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 argu- ments are curried).
  The first argument should be applied to elements of the second argument in order until the first time it returns
  SOME v for some v and then v is the result of the call to first_answer. If the first argument returns NONE for all
  list elements, then first_answer should raise the exception NoAnswer. Hints: Sample solution is 5 lines and does
  nothing fancy.
    */
  def firstAnswer[A, B](fn: (A) => Option[B])(items: Seq[A]): B = items match {
    case Nil => throw new NoAnswer
    case head :: tail => fn(head) match {
      case None => firstAnswer(fn)(tail)
      case Some(value) => value
    }
  }


  /**
  8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option (notice the 2 arguments
  are curried). The first argument should be applied to elements of the second argument. If it returns NONE for any
  element, then the result for all_answers is NONE. Else the calls to the first argument will have produced SOME lst1,
  SOME lst2, ... SOME lstn and the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended
  together (order doesn’t matter). Hints: The sample solution is 8 lines. It uses a helper function with an accumulator
  and uses @. Note all_answers f [] should evaluate to SOME [].
    */
  def allAnswers[A, B](fn: (A) => Option[Seq[B]])(items: Seq[A]): Option[Seq[B]] = {
    def allAnswersHelper(remaining: Seq[A], acc: Option[Seq[B]]):  Option[Seq[B]]  = remaining match {
      case Nil => acc
      case head :: tail => fn(head) match {
        case None => None
        case Some(value) => allAnswersHelper(tail, acc.map(_ ++ value))
      }
    }

    allAnswersHelper(items, Some(Seq.empty))
  }

  /**
  9a. Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it contains.
    */
  def countWildcards(pattern: Pattern): Int = g(() => 1, varValue => 0, pattern)


  /**
  9b. Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns the number of
  Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it
  contains. (Use String.size. We care only about variable names; the constructor names are not relevant.)
    */
  def countWildAndVariableLengths(pattern: Pattern): Int = g(() => 1, _.size, pattern)


  /**
  9c. Use g to define a function count_some_var that takes a string and a pattern (as a pair) and returns the number of
  times the string appears as a variable in the pattern. We care only about variable names; the constructor names are
  not relevant.
    */
  def countSomeVar(str: String, pattern: Pattern): Int = g(() => 0, varValue => if (varValue == str) 1 else 0, pattern)


  /**
  10. Write a function check_pat that takes a pattern and returns true if and only if all the variables appearing in
  the pattern are distinct from each other (i.e., use different strings). The constructor names are not relevant.
  Hints: The sample solution uses two helper functions. The first takes a pattern and returns a list of all the strings
  it uses for variables. Using foldl with a function that uses append is useful in one case. The second takes a list of
  strings and decides if it has repeats. List.exists may be useful. Sample solution is 15 lines. These are hints:
  We are not requiring foldl and List.exists here, but they make it easier.
    */
  def checkPat(pattern: Pattern): Boolean = false

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
