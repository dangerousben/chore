import cats.Eq
import cats.implicits._
import cats.effect.laws.discipline.SyncTests
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop._

object ChoreSpec extends Properties("Chore") {
  import Chore._

  def genChore[A](implicit A: Arbitrary[A]): Gen[Chore[A]] = Gen.lzy(Gen.oneOf(
    A.arbitrary.map(Complete(_)),
    Gen.alphaStr.map(s => Error(new java.lang.Exception(s))),
    genChore[A].map(fa => Suspend(() => fa)),
  ))
  implicit def arbChore[A](implicit A: Arbitrary[A]): Arbitrary[Chore[A]] = Arbitrary(genChore[A])

  implicit val eqT: Eq[Throwable] = Eq.by[Throwable, String](_.toString)
  implicit def eqChore[A](implicit eqA: Eq[A]): Eq[Chore[A]] = new Eq[Chore[A]] {
    def eqv(x: Chore[A], y: Chore[A]): Boolean = unsafeRunSync(x.attempt) === unsafeRunSync(y.attempt)
  }

  property("syncLaws") = all(SyncTests[Chore].sync[Int, Int, Int].all.properties.map {
    case (id, prop) => id |: prop
  } : _*)
}
