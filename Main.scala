import cats.free.{Free, Coyoneda}
import cats.{~>}
import shapeless._
import ops.coproduct.{Inject, Selector}
import Shopoyo._

sealed trait AskA[A]
case class AskMsg(msg: String) extends AskA[String]

object AskInterpreter extends (AskA ~> Id){
  def apply[A](a: AskA[A]) = a match {
    case AskMsg(msg) =>
      println(msg)
      scala.io.StdIn.readLine
  }
}

sealed trait TellA[A]
case class TellMsg(msg: String) extends TellA[Unit]

object TellInterpreter extends (TellA ~> Id){
  def apply[A](a: TellA[A]) = a match {
    case TellMsg(msg) =>
      println(msg)
  }
}

object Main extends App{

  type MyApp[A] = AskA[A] :+: TellA[A] :+: CNil
  type CoyoApp[A] = Coyoneda[MyApp, A]

  object Asker{
    def ask(msg: String) = Copoyo[MyApp](AskMsg(msg))
  }

  object Teller{
    def tell(msg: String) = Copoyo[MyApp](TellMsg(msg))
  }

  val prog = for{
    name <- Asker.ask("What's your name?")
    _    <- Teller.tell(s"Hi $name")
  } yield ()

  val interpreter: MyApp ~> Id = AskInterpreter combine TellInterpreter
  val interpreterCoyo: CoyoApp ~> Id = liftCoyoLeft(interpreter)

  prog.foldMap(interpreterCoyo)

}
