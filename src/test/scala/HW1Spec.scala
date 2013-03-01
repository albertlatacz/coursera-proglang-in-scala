import org.specs2.mutable.Specification
import HW1._

class HW1Spec extends Specification {
  "isOlder returns correct value" >> {
    isOlder((2011, 12, 10), (2011, 12, 5)) shouldEqual true
    isOlder((2011, 12, 10), (2012, 11, 5)) shouldEqual false
    isOlder((2011, 12, 5), (2011, 12, 10)) shouldEqual false
    isOlder((2011, 12, 10), (2011, 12, 10)) shouldEqual false
  }

  "numberInMonth returns number of dates from the list with a given month" >> {
    numberInMonth(Seq((2011, 12, 10), (2011, 11, 10), (2011, 12, 5)), 12) shouldEqual 2
  }

  "numberInMonths returns number of dates from the list with a given months from a list" >> {
    numberInMonths(Seq((2011, 12, 10), (2011, 11, 10), (2011, 12, 5), (2011, 10, 7)), Seq(12, 10)) shouldEqual 3
  }

  "datesInMonth returns dates from the list with a given month" >> {
    datesInMonth(Seq((2011, 12, 10), (2011, 11, 10), (2011, 12, 5)), 12) shouldEqual Seq((2011, 12, 10), (2011, 12, 5))
  }

  "datesInMonths returns dates from the list with a given months from a list" >> {
    datesInMonths(Seq((2011, 12, 10), (2011, 11, 10), (2011, 10, 7)), Seq(12, 10)) shouldEqual Seq((2011, 12, 10), (2011, 10, 7))
  }

  "getNth returns nth element of the list of strings" >> {
    getNth(Seq("hello", "there", "big", "world", "!"), 4) shouldEqual "world"
    getNth(Seq("this", "should", "return", "null"), -1) shouldEqual null
    getNth(Seq("and", "this", "also", "returns", "null"), 10) shouldEqual null
  }
}
