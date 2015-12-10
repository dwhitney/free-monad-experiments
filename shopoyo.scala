import scala.language.higherKinds

import shapeless.ops.coproduct.{Inject, Selector}
import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1, Id, DepFn1}

import cats.{Functor, Unapply, ~>}
import cats.free.Free
import cats.free.Coyoneda

object Shopoyo{

  type FreeC[S[_], A] = Free[({type f[x] = Coyoneda[S, x]})#f, A]
  type CoyonedaF[F[_]] = ({type A[α] = Coyoneda[F, α]})

  /** Lifts NatTrans to NatTrans of NatTrans of Coyoneda */
  def liftCoyo[F[_], G[_]](fg: F ~> G): CoyonedaF[F]#A ~> CoyonedaF[G]#A =
    new (CoyonedaF[F]#A ~> CoyonedaF[G]#A) {
      def apply[A](c: Coyoneda[F, A]) = {
        Coyoneda.apply(fg(c.fi))(c.k)
      }
    }

  def liftCoyoLeft[F[_], G[_]: Functor](fg: F ~> G): CoyonedaF[F]#A ~> G = {
    type CF[A] = Coyoneda[F, A]
    type CG[A] = Coyoneda[G, A]

    val m: (CF ~> CG) = liftCoyo(fg)

    new (CF ~> G) {
      def apply[A](c: CF[A]) = m(c).run
    }
  }

  /** A version of `liftF` that infers the nested type constructor. */
  def liftFU[MA](value: => MA)(implicit MA: Unapply[Functor, MA]): Free[MA.M, MA.A] =
    Free.liftF(MA.subst(value))//(MA.TC)

  /** A free monad over a free functor of `S`. */
  def liftFC[S[_], A](s: S[A]): FreeC[S, A] =
    liftFU(Coyoneda.lift(s))

  /** Helper to inject a F[A] into Coproduct into Coyoneda into FreeMonad */
  class Copoyo[C[_] <: Coproduct] {
    def apply[F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): FreeC[C, A] =
      liftFC(Coproduct[C[A]](fa))
  }

  object Copoyo {
    def apply[C[_] <: Coproduct] = new Copoyo[C]
  }



  /** Coproduct Natural Transformations */
  implicit class RichNatT[F[_], G[_]](val nat: F ~> G) extends AnyVal {

    def combine[H[_]](f: H ~> G): ({type f[x] = F[x] :+: H[x] :+: CNil })#f ~> G =
      new (({type f[x] = F[x] :+: H[x] :+: CNil})#f ~> G){
        def apply[T](c: F[T] :+: H[T] :+: CNil): G[T] = c match {
          case Inl(h) => nat(h)
          case Inr(Inl(t)) => f(t)
          case _ => throw new RuntimeException("impossible case")
        }
      }
  }

  /** Coproduct Functors */
  implicit def CoproductFunctor1[F[_]](implicit F: Functor[F]) =
    new Functor[({ type l[A] = F[A] :+: CNil })#l] {

      def map[A, B](fa: F[A] :+: CNil)(f: A => B): F[B] :+: CNil = fa match {
        case Inl(h) => Coproduct[F[B] :+: CNil](F.map(h)(f))
        case Inr(t) => throw new RuntimeException("impossible case")
        case _ => throw new RuntimeException("impossible case")
      }

    }
}
