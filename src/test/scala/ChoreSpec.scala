import cats.Eq
import cats.implicits._
import cats.laws.discipline.DeferTests
import cats.effect.laws.discipline.BracketTests
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._

object ChoreSpec extends Properties("Chore") {
  import Chore._

  implicit val throwableEq: Eq[Throwable] = Eq.by[Throwable, String](_.toString)

  def genChore[A](implicit A: Arbitrary[A]): Gen[Chore[A]] = Gen.oneOf(
    A.arbitrary.map(Complete(_)),
    Gen.alphaStr.map(s => Error(new java.lang.Exception(s))),
    Gen.lzy(genChore[A].map(fa => Suspend(() => fa))),
  )

  implicit def arbChore[A](implicit A: Arbitrary[A]): Arbitrary[Chore[A]] = Arbitrary(genChore[A])

  property("deferLaws") = all(DeferTests[Chore].defer[Int].all.properties.map {
    case (id, prop) => id |: prop
  } : _*)

  property("bracketLaws") = all(BracketTests[Chore, Throwable].bracket[Int, Int, Int].all.properties.map {
    case (id, prop) => id |: prop
  } : _*)
}
