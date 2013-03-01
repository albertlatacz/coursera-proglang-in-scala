
object HW1 {
  def isOlder(firstDate: Tuple3[Int, Int, Int], secondDate: Tuple3[Int, Int, Int]): Boolean = {
    def dateToInt(date: Tuple3[Int, Int, Int]): Int = date._1 * 10000 + date._2 * 100 + date._3
    dateToInt(firstDate) > dateToInt(secondDate)
  }

  def numberInMonth(dates: Seq[Tuple3[Int, Int, Int]], month: Int): Int =
    if (dates.isEmpty) 0
    else (if (dates.head._2 == month) 1 else 0) + numberInMonth(dates.tail, month)


  def numberInMonths(dates: Seq[Tuple3[Int, Int, Int]], months: Seq[Int]): Int =
    if (months.isEmpty) 0
    else numberInMonth(dates, months.head) + numberInMonths(dates, months.tail)

  def datesInMonth(dates: Seq[Tuple3[Int, Int, Int]], month: Int): Seq[Tuple3[Int, Int, Int]] =
    if (dates.isEmpty) Seq.empty
    else {
      if (dates.head._2 == month) dates.head +: datesInMonth(dates.tail, month)
      else datesInMonth(dates.tail, month)
    }

  def datesInMonths(dates: Seq[Tuple3[Int, Int, Int]], months: Seq[Int]): Seq[Tuple3[Int, Int, Int]] =
    if (months.isEmpty) Seq.empty
    else datesInMonth(dates, months.head) ++ datesInMonths(dates, months.tail)

  def getNth(strings: Seq[String], index: Int): String =
    if (index > strings.size || index <= 0) null
    else if (index == 1) strings.head
    else getNth(strings.tail, index - 1)
}