import org.specs2.mutable.Specification
import HW2._

class HW2Spec extends Specification {

  "allExceptOption returns Some(strings) if given string is in the list otherwise None" >> {
    allExceptOption("Hello", Seq("Hello", "big", "world!")) shouldEqual Some(Seq("big", "world!"))
    allExceptOption("big", Seq("Hello", "big", "world!")) shouldEqual Some(Seq("Hello", "world!"))
    allExceptOption("not matching", Seq("Hello", "big", "world!")) shouldEqual None
  }

  "getSubstitutions1 returns substitutions for a given list of strings" >> {
    getSubstitutions1(Seq(Seq("Fred", "Fredrick"), Seq("Elizabeth", "Betty"), Seq("Freddie", "Fred", "F")), "Fred") shouldEqual Seq("Fredrick", "Freddie", "F")
    getSubstitutions1(Seq(Seq("Fred", "Fredrick"), Seq("Jeff", "Jeffrey"), Seq("Geoff", "Jeff", "Jeffrey")), "Jeff") shouldEqual Seq("Jeffrey", "Geoff", "Jeffrey")
  }

  "getSubstitutions2 returns substitutions for a given list of strings using tail recursive local helper" >> {
    getSubstitutions2(Seq(Seq("Fred", "Fredrick"), Seq("Elizabeth", "Betty"), Seq("Freddie", "Fred", "F")), "Fred") shouldEqual Seq("Fredrick", "Freddie", "F")
    getSubstitutions2(Seq(Seq("Fred", "Fredrick"), Seq("Jeff", "Jeffrey"), Seq("Geoff", "Jeff", "Jeffrey")), "Jeff") shouldEqual Seq("Jeffrey", "Geoff", "Jeffrey")
  }

  "similarNames returns all substitutions of the first name of given person" >> {
    similarNames(Seq(Seq("Fred", "Fredrick"), Seq("Elizabeth", "Betty"), Seq("Freddie", "Fred", "F")), new Person(first = "Fred", last = "Smith", middle = "W")) shouldEqual
      Seq(new Person(first = "Fred", last = "Smith", middle = "W"),
        new Person(first = "Fredrick", last = "Smith", middle = "W"),
        new Person(first = "Freddie", last = "Smith", middle = "W"),
        new Person(first = "F", last = "Smith", middle = "W"))

    similarNames(Seq(Seq("Fred", "Fredrick"), Seq("Elizabeth", "Betty"), Seq("Freddie", "Fred", "F")), new Person(first = "John", last = "West", middle = "S")) shouldEqual
      Seq(new Person(first = "John", last = "West", middle = "S"))
  }

  "cardColor returns color (Black, Red) of a given card" >> {
    cardColor((Hearts, Ace)) shouldEqual Red
    cardColor((Spades, Ace)) shouldEqual Black
    cardColor((Diamonds, Ace)) shouldEqual Red
    cardColor((Clubs, Ace)) shouldEqual Black
  }

  "cardValue returns value of Num, 11 for Ace and 10 otherwise" >> {
    cardValue((Spades, Ace)) shouldEqual 11
    cardValue((Spades, King)) shouldEqual 10
    cardValue((Spades, Queen)) shouldEqual 10
    cardValue((Spades, Jack)) shouldEqual 10
    cardValue((Spades, Num(3))) shouldEqual 3
  }

  "removeCard removes card from the list or throws given exception if card was not found" >> {
      removeCard(Seq((Diamonds, King), (Spades, Ace), (Spades, Ace), (Hearts, Queen)), (Spades, Ace), null) shouldEqual Seq((Diamonds, King), (Spades, Ace), (Hearts, Queen))
      removeCard(Seq((Spades, Ace), (Diamonds, King)), (Hearts, Jack), new IllegalArgumentException) should throwAn(new IllegalArgumentException)
    }

}
