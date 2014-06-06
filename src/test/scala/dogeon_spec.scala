import org.specs2.mutable._
import dogeon.Dogeon

class DogeonSpec extends Specification {


  "Much Dogeon" should {
    "parse many" in {

      val dogeString: String = "such \"foo\" is so \"bar\" next \"baz\" next \"fizzbuzz\" many wow"

      val result = Dogeon.muchWow(dogeString)

      val dogeMap: Map[String, List[String]] = result.asInstanceOf[Map[String, List[String]]]
      dogeMap("foo") === List("bar", "baz", "fizzbuzz")

      val maybeDogeList : List[String]  = dogeMap("foo")

      maybeDogeList(0) === "bar"
      maybeDogeList(1) === "baz"
      maybeDogeList(2) === "fizzbuzz"


    }

    "so much" in {

      val dogeString: String = """such "foo" is "bar" wow"""

      val result = Dogeon.muchWow(dogeString)

      val dogeMap: Map[String, String] = result.asInstanceOf[Map[String, String]]

      dogeMap("foo") === "bar"
    }

  }

}
