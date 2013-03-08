
object HW2 extends HW2Provided {
  /**
  1a. Write a function all_except_option, which takes a string and a string list. Return NONE if the string is not in the
  list, else return SOME lst where lst is identical to the argument list except the string is not in it. You may assume
  the string is in the list at most once. Use same_string, provided to you, to compare strings. Sample solution is
  around 8 lines. */
  def allExceptOption(excluded: String, list: Seq[String]): Option[Seq[String]] = list match {
    case Nil => None
    case head :: tail =>
      if (sameString(excluded, head)) Some(tail)
      else allExceptOption(excluded, tail).map(head +: _)
  }


  /**
  1b. Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions)
  and a string s and returns a string list. The result has all the strings that are in some list in substitutions that
  also has s, but s itself should not be in the result. Example:
    get_substitutions1([ ["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"] ]
                       "Fred")
    answer: ["Fredrick","Freddie","F"]

  Assume each list in substitutions has no repeats. The result will have repeats if s and another string are both in
  more than one list in substitutions. Example:
    get_substitutions1([ ["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", " Jeffrey"] ],
                       "Jeff")
    answer: ["Jeffrey","Geoff","Jeffrey"]

  Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines.
    */
  def getSubstitutions1(list: Seq[Seq[String]], substitute: String): Seq[String] = list match {
    case Nil => Seq.empty
    case head :: tail => allExceptOption(substitute, head) match {
      case None => getSubstitutions1(tail, substitute)
      case Some(result) => result ++ getSubstitutions1(tail, substitute)
    }
  }


  /**
  1c. Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local
  helper function.
    */
  def getSubstitutions2(list: Seq[Seq[String]], substitute: String): Seq[String] = {
    def getSubstitutionsTailRec(remaining: Seq[Seq[String]], acc: Seq[String]): Seq[String] = remaining match {
      case Nil => acc
      case head :: tail => getSubstitutionsTailRec(tail, acc ++ allExceptOption(substitute, head).getOrElse(Seq.empty))
    }

    getSubstitutionsTailRec(list, Seq.empty)
  }

  /**
  Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full
  name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list).
  The result is all the full names you can produce by substituting for the first name (and only the first name) using
  substitutions and parts (b) or (c). The answer should begin with the original name (then have 0 or more other names).
  Example: similar_names([ ["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"] ], {first="Fred", middle="W", last="Smith"})
    answer: [{first="Fred", last="Smith", middle="W"},
      {first="Fredrick", last="Smith", middle="W"},
      {first="Freddie", last="Smith", middle="W"},
      {first="F", last="Smith", middle="W"}]
  Hint: Use a local helper function. Sample solution is around 10 lines.

    Since Scala has no equivalent type for records this problem has been solved with helper type 'Person'.
    */
  case class Person(first: String, last: String, middle: String)

  def similarNames(list: Seq[Seq[String]], person: Person): Seq[Person] = {
    def similarNamesTailRec(names: Seq[String], acc: Seq[Person]): Seq[Person] = names match {
      case Nil => acc
      case head :: tail => similarNamesTailRec(tail, acc :+ person.copy(first = head))
    }

    person +: similarNamesTailRec(getSubstitutions2(list, person.first), Seq.empty)
  }


  /**
  2a. Write a function card_color, which takes a card and returns its color (spades and clubs are black,
  diamonds and hearts are red). Note: One case-expression is enough.
    */
  def cardColor(card: Card): Color = card match {
    case (Hearts | Diamonds, _) => Red
    case _ => Black
  }

  /**
  2b. Write a function card_value, which takes a card and returns its value (numbered cards have their
  number as the value, aces are 11, everything else is 10). Note: One case-expression is enough.
    */
  def cardValue(card: Card): Int = card match {
    case (_, Num(value)) => value
    case (_, Ace) => 11
    case _ => 10
  }

}

/**
Provided originally from hw2provided.sml
  */
class HW2Provided {
  // For problem 1
  /**
  If you use this function to compare two strings (returns true if the same string), then you avoid several of
  the functions in problem 1 having polymorphic types that may be confusing
    */
  def sameString(s1: String, s2: String) = s1 == s2


  // For problem 2
  abstract sealed class Suit
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit

  abstract sealed class Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank
  case class Num(num: Int) extends Rank // you may assume that Num is always used with values 2, 3, ..., 10 though it will not really come up

  type Card = (Suit, Rank)

  abstract sealed class Color
  case object Red extends Color
  case object Black extends Color

  abstract sealed class Move
  case class Discard(card: Card)
  case object Draw

  class IllegalMove extends Exception

}
