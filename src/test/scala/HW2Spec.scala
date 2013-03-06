import org.specs2.mutable.Specification
import HW2._

class HW2Spec extends Specification {

  "allExceptOption returns Some(strings) if given string is in the list otherwise None" >> {
     allExceptOption("Hello", Seq("Hello", "big", "world!")) shouldEqual Some(Seq("big", "world!"))
     allExceptOption("big", Seq("Hello", "big", "world!")) shouldEqual Some(Seq("Hello", "world!"))
     allExceptOption("not matching", Seq("Hello", "big", "world!")) shouldEqual None
  }

}
