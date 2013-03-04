import org.specs2.mutable.Specification
import HW1._

class HW1Spec extends Specification {

  "isOlder returns correct value" >> {
    isOlder((2011, 12, 10), (2011, 12, 5)) shouldEqual false
    isOlder((2011, 12, 10), (2012, 11, 5)) shouldEqual true
    isOlder((2011, 12, 5), (2011, 12, 10)) shouldEqual true
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

  "dateToString returns date as string in a format 'January 20, 2013'" >> {
    dateToString((1984, 3, 25)) shouldEqual "March 25, 1984"
  }

  "numberBeforeReachingSum returns number of items from a list which, when summed are less than specified number" >> {
    numberBeforeReachingSum(7, Range(1, 10)) shouldEqual 3
  }

  "whatMonth returns number of a month for a given day of the year" >> {
    whatMonth(12) shouldEqual 1
    whatMonth(65) shouldEqual 3
    whatMonth(365) shouldEqual 12
  }

  "monthRange returns list of months for a given days range" >> {
    monthRange(100, 0) shouldEqual Seq.empty
    monthRange(10, 12) shouldEqual Seq(1, 1, 1)
    monthRange(58, 62) shouldEqual Seq(2, 2, 3, 3, 3)
  }

  "oldest returns the oldest date" >> {
    oldest(Seq.empty) shouldEqual None
    oldest(Seq((1984, 3, 25))) shouldEqual Some((1984, 3, 25))
    oldest(Seq((1984, 3, 25), (1983, 9, 11), (2011, 12, 10), (2011, 11, 10))) shouldEqual Some((1983, 9, 11))
  }

  "numberInMonthsChallenge returns number of dates from the list with a given months from a list making sure there are no duplicates" >> {
    numberInMonthsChallenge(Seq((2011, 12, 10), (2011, 11, 10), (2011, 12, 5), (2011, 10, 7)), Seq(12, 10, 10, 12)) shouldEqual 3
  }

  "datesInMonthsChallenge returns dates from the list with a given months from a list making sure there are no duplicates" >> {
    datesInMonthsChallenge(Seq((2011, 12, 10), (2011, 11, 10), (2011, 10, 7)), Seq(12, 10, 10, 12)) shouldEqual Seq((2011, 12, 10), (2011, 10, 7))
  }

}
