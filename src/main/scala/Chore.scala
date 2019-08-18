import cats.Eq
import cats.effect.{ Sync, ExitCase }
import scala.annotation.tailrec
import scala.util.control.NonFatal

sealed trait Chore[+A]

object Chore {
  case class Complete[A](value: A) extends Chore[A]
  case class Error(error: Throwable) extends Chore[Nothing]
  case class Suspend[A](thunk: () => Chore[A]) extends Chore[A]

  @tailrec
  def unsafeRunSync[A](fa: Chore[A]): A = fa match {
    case Complete(a) => a
    case Error(e) => throw e
    case Suspend(thunk) => unsafeRunSync(thunk())
  }

  implicit val syncInstance: Sync[Chore] = new Sync[Chore] {

    // MonadError

    def pure[A](a: A): Chore[A] = Complete(a)

    def flatMap[A, B](fa: Chore[A])(f: A => Chore[B]): Chore[B] = fa match {
      case Complete(a) => f(a)
      case Error(_) => fa.asInstanceOf[Chore[B]]
      case Suspend(thunk) => Suspend(() => flatMap(thunk())(f))
    }

    def tailRecM[A, B](a: A)(f: A => Chore[Either[A,B]]): Chore[B] = {
      @tailrec
      def loop(v: Chore[Either[A, B]]): Chore[B] = v match {
        case Complete(Right(b)) => Complete(b)
        case Complete(Left(a)) => loop(f(a))
        case Error(_) => v.asInstanceOf[Chore[B]]
        case Suspend(thunk) => loop(thunk())
      }
      loop(f(a))
    }

    def handleErrorWith[A](fa: Chore[A])(f: Throwable => Chore[A]): Chore[A] = fa match {
      case Error(e) => f(e)
      case _ => fa
    }

    def raiseError[A](e: Throwable): Chore[A] = Error(e)


    // Bracket

    def bracketCase[A, B](acquire: Chore[A])(use: A => Chore[B])(release: (A, ExitCase[Throwable]) => Chore[Unit]): Chore[B] =
      flatMap(acquire) { a =>
        val result = use(a)
        result match {
          case Complete(_) => release(a, ExitCase.Completed)
          case Error(e) => release(a, ExitCase.Error(e))
          case Suspend(thunk) => Suspend(() => bracketCase(Complete(a))(_ => thunk())(release))
        }
        result
      }


    // Sync

    def suspend[A](fa: => Chore[A]): Chore[A] = Suspend(() => try fa catch { case NonFatal(e) => Error(e) })
  }
}
