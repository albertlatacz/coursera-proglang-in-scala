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



}
