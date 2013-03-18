import org.specs2.mutable.Specification
import HW3._

class HW3Spec extends Specification {

  "onlyCapitals returns list of strings starting with capital letter" >> {
    onlyCapitals(Seq("Hello", "tHeRe", "big", "World!")) shouldEqual Seq("Hello", "World!")
  }

  "longestString1 returns the longest string in the given list of strings" >> {
    longestString1(Seq("Returns", "the longest", "string from", "the", "list")) shouldEqual "the longest"
    longestString1(Seq.empty) shouldEqual ""
  }

  "longestString2 returns the longest string in the given list of strings from the right" >> {
    longestString2(Seq("Returns", "the longest", "string from", "the", "list")) shouldEqual "string from"
    longestString2(Seq.empty) shouldEqual ""
  }

  "longestString3 returns the longest string in the given list of strings using helper curried function" >> {
    longestString3(Seq("Returns", "the longest", "string from", "the", "list")) shouldEqual "the longest"
    longestString3(Seq.empty) shouldEqual ""
  }

  "longestString4 returns the longest string in the given list of strings from the right using helper curried function" >> {
    longestString4(Seq("Returns", "the longest", "string from", "the", "list")) shouldEqual "string from"
    longestString4(Seq.empty) shouldEqual ""
  }

  "longestCapitalized returns the longest string starting with capital letter" >> {
    longestCapitalized(Seq("Returns", "the longest", "Capitalised", "string")) shouldEqual "Capitalised"
    longestCapitalized(Seq.empty) shouldEqual ""
  }

  "revString reverses the string" >> {
    revString("Some String") shouldEqual "gnirtS emoS"
  }

  "firstAnswer returns value of the first results of calling fn that return Some(value) or throws NoAnswer if all None" >> {
    def evenOption(value: Integer) = if (value % 2 == 0) Some(value) else None
    firstAnswer(evenOption)(Seq(1, 3, 4, 5, 6)) shouldEqual 4
    firstAnswer(evenOption)(Seq(1, 3, 5, 7, 9)) should throwA[NoAnswer]
  }

  "allAnswers returns list containing all results of calling fn that return Some(value) or None if any None" >> {
    def evenOption(value: Integer) = if (value % 2 == 0) Some(Seq(value, value)) else None
    allAnswers(evenOption)(Seq(4, 6)) shouldEqual Some(Seq(4, 4, 6, 6))
    allAnswers(evenOption)(Seq(4, 3)) shouldEqual None
    allAnswers(evenOption)(Seq.empty) shouldEqual Some(Seq.empty)
  }

}
