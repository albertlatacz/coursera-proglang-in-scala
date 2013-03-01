
object HW1 {
  /**
  1. Write a function is_older that takes two dates and evaluates to true or false.
  It evaluates to true if the first argument is a date that comes before the second argument.
  (If the two dates are the same, the result is false.)
    */
  def isOlder(firstDate: Tuple3[Int, Int, Int], secondDate: Tuple3[Int, Int, Int]): Boolean = {
    def dateToInt(date: Tuple3[Int, Int, Int]): Int = date._1 * 10000 + date._2 * 100 + date._3
    dateToInt(firstDate) < dateToInt(secondDate)
  }


  /**
  2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and
  returns how many dates in the list are in the given month.
    */
  def numberInMonth(dates: Seq[Tuple3[Int, Int, Int]], month: Int): Int =
    if (dates.isEmpty) 0
    else (if (dates.head._2 == month) 1 else 0) + numberInMonth(dates.tail, month)


  /**
  3. Write a function number_in_months that takes a list of dates and a list of months (i.e.,
  an int list) and returns the number of dates in the list of dates that are in any of the months
  in the list of months. Assume the list of months has no number repeated. Hint: Use your answer
  to the previous problem.
    */
  def numberInMonths(dates: Seq[Tuple3[Int, Int, Int]], months: Seq[Int]): Int =
    if (months.isEmpty) 0
    else numberInMonth(dates, months.head) + numberInMonths(dates, months.tail)


  /**
  4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and
  returns a list holding the dates from the argument list of dates that are in the month.
  The returned list should contain dates in the order they were originally given.
    */
  def datesInMonth(dates: Seq[Tuple3[Int, Int, Int]], month: Int): Seq[Tuple3[Int, Int, Int]] =
    if (dates.isEmpty) Seq.empty
    else {
      if (dates.head._2 == month) dates.head +: datesInMonth(dates.tail, month)
      else datesInMonth(dates.tail, month)
    }


  /**
  5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
  and returns a list holding the dates from the argument list of dates that are in any of the months in
  the list of months. Assume the list of months has no number repeated.
    */
  def datesInMonths(dates: Seq[Tuple3[Int, Int, Int]], months: Seq[Int]): Seq[Tuple3[Int, Int, Int]] =
    if (months.isEmpty) Seq.empty
    else datesInMonth(dates, months.head) ++ datesInMonths(dates, months.tail)


  /**
  6. Write a function get_nth that takes a list of strings and an int n and
  returns the nth element of the list where the head of the list is 1st.
  Do not worry about the case where the list has too few elements: your
  function may apply hd or tl to the empty list in this case, which is okay.
    */
  def getNth(strings: Seq[String], index: Int): String =
    if (index > strings.size || index <= 0) null
    else if (index == 1) strings.head
    else getNth(strings.tail, index - 1)


  /**
  7. Write a function date_to_string that takes a date and returns a string of
  the form January 20, 2013 (for example). Use the operator for concatenating
  strings and the library function Int.toString for converting an int to a string.
  For producing the month part, do not use a bunch of conditionals. Instead, use a
  list holding 12 strings and your answer to the previous problem. For consistency,
  put a comma following the day and use capitalized English month names: January,
  February, March, April, May, June, July, August, September, October, November, December.
    */
  def dateToString(date: Tuple3[Int, Int, Int]): String =
    "%s %d, %d".format(
      Seq("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")(date._2 - 1),
      date._3,
      date._1)


  /**
  8. Write a function number_before_reaching_sum that takes an int called sum, which you
  can assume is positive, and an int list, which you can assume contains all positive
  numbers, and returns an int. You should return an int n such that the first n elements
  of the list add to less than sum, but the first n + 1 elements of the list add to sum
  or more. Assume the entire list sums to more than the passed in value; it is okay for
  an exception to occur if this is not the case.
    */
  def numberBeforeReachingSum(sum: Int, numbers: Seq[Int]): Int =
    if (sum - numbers.head <= 0) 0
    else 1 + numberBeforeReachingSum(sum - numbers.head, numbers.tail)


  /**
  9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365)
  and returns what month that day is in (1 for January, 2 for February, etc.). Use a list
  holding 12 integers and your answer to the previous problem.
    */
  def whatMonth(dayOfYear: Int): Int =
    1 + numberBeforeReachingSum(dayOfYear, Seq(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))


  /**
  10. Write a function month_range that takes two days of the year day1 and day2 and returns an
  int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn
  is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1 > day2.
    */
  def monthRange(fromDay: Int, toDay: Int): Seq[Int] =
    if (fromDay > toDay) Seq.empty
    else whatMonth(fromDay) +: monthRange(fromDay + 1, toDay)


  /**
  11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option.
  It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
    */
  def oldest(dates: Seq[Tuple3[Int, Int, Int]]): Option[Tuple3[Int, Int, Int]] = {
    def findOldest(currentOldest: Tuple3[Int, Int, Int], remainingDates: Seq[Tuple3[Int, Int, Int]]): Tuple3[Int, Int, Int] = {
      if (remainingDates.isEmpty) currentOldest
      else if (isOlder(remainingDates.head, currentOldest)) findOldest(remainingDates.head, remainingDates.tail)
      else findOldest(currentOldest, remainingDates.tail)
    }

    if (dates.isEmpty) None
    else Some(findOldest(dates.head, dates.tail))
  }

}
