import org.specs2.mutable.Specification
import HW3._

class HW3Spec extends Specification {

  "onlyCapitals returns list of strings starting with capital letter" >> {
    onlyCapitals(Seq("Hello", "tHeRe", "big", "World!")) shouldEqual Seq("Hello", "World!")
  }

}
