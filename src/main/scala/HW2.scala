
object HW2 {

  // Provided originally from hw2provided.sml

  // For problem 1
  /**
  If you use this function to compare two strings (returns true if the same string), then you avoid several of
  the functions in problem 1 having polymorphic types that may be confusing */

  def sameString(s1: String, s2: String) = s1 == s2

  /**
  1a. Write a function all_except_option, which takes a string and a string list. Return NONE if the string is not in the
  list, else return SOME lst where lst is identical to the argument list except the string is not in it. You may assume
  the string is in the list at most once. Use same_string, provided to you, to compare strings. Sample solution is
  around 8 lines.*/
  def allExceptOption(excluded: String, list: Seq[String]): Option[Seq[String]] = list match {
    case Nil => None
    case head :: tail if (excluded == head) => Some(tail)
    case head :: tail => allExceptOption(excluded, tail).map(head +: _)
  }


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
