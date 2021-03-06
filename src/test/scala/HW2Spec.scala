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

  "allSameColor returns true for list of cards with the same color" >> {
    allSameColor(Seq((Spades, Ace), (Diamonds, King), (Spades, Queen))) shouldEqual false
    allSameColor(Seq((Spades, Ace), (Spades, King), (Spades, Queen))) shouldEqual true
  }

  "sumCards returns sum of values of given list of cards" >> {
    sumCards(Seq((Spades, Ace), (Diamonds, King), (Hearts, Num(3)), (Diamonds, Num(6)))) shouldEqual 30
  }

  "score returns correct score for cards held" >> {
    score(Seq((Spades, Ace), (Hearts, Num(4)), (Diamonds, King)), 20) shouldEqual 55
    score(Seq((Spades, Ace), (Hearts, Num(4))), 20) shouldEqual 5
    score(Seq((Hearts, Ace), (Hearts, Num(4)), (Hearts, King)), 20) shouldEqual 27
    score(Seq((Hearts, Ace), (Hearts, Num(4))), 20) shouldEqual 2
  }

  "officiate " should {
    "end the game when no more moves" >> {
      officiate(Seq((Hearts, Ace), (Diamonds, Ace)), Seq(Draw), 20) shouldEqual 4
    }

    "end the game when no more cards to draw" >> {
      officiate(Seq((Hearts, Ace)), Seq(Draw, Draw), 20) shouldEqual 4
    }

    "end the game when sum of cards held is greater than goal" >> {
      officiate(Seq((Hearts, Ace), (Spades, King), (Clubs, Jack)), Seq(Draw, Draw, Draw), 20) shouldEqual 43
    }

    "discard a card if in list of cards held" >> {
      officiate(Seq((Hearts, Ace), (Spades, Num(2))), Seq(Draw, Draw, Discard((Spades, Num(2)))), 20) shouldEqual 4
    }

    "throw 'IllegalMove' if when trying to discard card not currently held" >> {
      officiate(Seq((Hearts, Ace), (Spades, Num(2))), Seq(Draw, Discard((Diamonds, Queen))), 20) should throwAn[IllegalMove]
    }
  }

  "scoreChallenge returns the smallest score where Aces can be 1 or 11" >> {
    scoreChallenge(Seq((Hearts, Num(4)), (Spades, Ace), (Diamonds, King)), 10) shouldEqual 35
    scoreChallenge(Seq((Hearts, Num(4)), (Spades, Ace)), 10) shouldEqual 5
  }

  "officiateChallenge returns the smallest score where Aces can be 1 or 11" >> {
    officiateChallenge(Seq((Hearts, Ace), (Spades, Ace), (Clubs, Ace)), Seq(Draw, Draw, Draw), 40) shouldEqual 7
    officiateChallenge(Seq((Hearts, Ace), (Spades, Ace), (Clubs, Ace)), Seq(Draw, Draw, Draw), 20) shouldEqual 17
  }

  "carefulPlayer returns correct result" >> {
    carefulPlayer(Seq((Hearts, Ace), (Spades, Ace)), 20) shouldEqual Seq(Draw, Discard((Hearts, Ace)), Draw)
  }


}
