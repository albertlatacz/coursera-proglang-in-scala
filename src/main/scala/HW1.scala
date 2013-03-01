object HW1 {
  def isOlder(firstDate: Tuple3[Int, Int, Int], secondDate: Tuple3[Int, Int, Int]): Boolean = {
    def dateToInt(date: Tuple3[Int, Int, Int]) : Int = date._1 * 10000 + date._2 * 100 + date._3
    dateToInt(firstDate) > dateToInt(secondDate)
  }

  def numberInMonth(dates: Seq[Tuple3[Int, Int, Int]], month: Int): Int =
    if (dates.isEmpty) 0
    else (if (dates.head._2 == month) 1 else 0) + numberInMonth(dates.tail, month)


  def numberInMonths(dates: Seq[Tuple3[Int, Int, Int]], months: Seq[Int]): Int =
    if (months.isEmpty) 0
    else numberInMonth(dates, months.head) + numberInMonths(dates, months.tail)

}