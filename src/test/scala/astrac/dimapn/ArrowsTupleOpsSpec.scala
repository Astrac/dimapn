package astrac.dimapn

import astrac.dimapn.syntax._
import cats.implicits._
import utest._

object ArrowsTupleOpsSpec extends TestSuite {

  val tests = Tests {
    "mergeMapN" - {
      val intToString = ((_: Int).toString)
      val intIsOdd = ((_: Int) % 2 == 1)
      val doubleInt = ((_: Int) * 2)
      val boolToString = ((_: Boolean).toString)

      val f: Int => String = (
        intToString,
        intIsOdd >>> boolToString,
        doubleInt >>> intToString
      ).mergeMapN((i, o, d) => s"$i | $o | $d")

      assert(
        f(1) == "1 | true | 2",
        f(4) == "4 | false | 8",
        f(17) == "17 | true | 34"
      )
    }

    "dimapN" - {
      case class BuildString(segment: String, separator: String, count: Int)

      val f: BuildString => String = (
        (identity[String] _),
        (identity[String] _),
        (identity[Int] _)
      ).dimapN(
        (f: BuildString) => (f.segment, f.separator, f.count))(
        (seg, sep, cnt) => List.fill(cnt)(seg).mkString(sep))

      assert(
        f(BuildString("*", "|", 4)) == "*|*|*|*",
        f(BuildString("[]", "*", 3)) == "[]*[]*[]",
        f(BuildString("|", "***", 1)) == "|"
      )
    }
  }
}
