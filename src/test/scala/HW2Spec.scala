import org.specs2.mutable.Specification
import HW2._

class HW2Spec extends Specification {

  "allExceptOption returns Some(strings) if given string is in the list otherwise None" >> {
    allExceptOption("Hello", Seq("Hello", "big", "world!")) shouldEqual Some(Seq("big", "world!"))
    allExceptOption("big", Seq("Hello", "big", "world!")) shouldEqual Some(Seq("Hello", "world!"))
    allExceptOption("not matching", Seq("Hello", "big", "world!")) shouldEqual None
  }

  "getSubstitutions1 returns substitutions for a given list of strings" >> {
    getSubstitutions1(Seq(Seq("Fred", "Fredrick"), Seq("Elizabeth", "Betty"), Seq("Freddie", "Fred", "F")), "Fred") shouldEqual Seq("Fredrick","Freddie","F")
    getSubstitutions1(Seq(Seq("Fred", "Fredrick"), Seq("Jeff", "Jeffrey"), Seq("Geoff", "Jeff", "Jeffrey")), "Jeff") shouldEqual Seq("Jeffrey", "Geoff", "Jeffrey")
  }

}
