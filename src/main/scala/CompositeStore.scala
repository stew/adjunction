package adjunction

import cats._
import cats.data._

object CompositeStore {

  import Adjunction._

  /**
    * Coreader ⊣ Reader gives rise to a comonad when composed as
    * `Writer[S, Reader[S, α]] => α`, and the Comonad is for `(s, s => α)` which
    * looks just like the Store Comonad!
    **/

  type Store[S, A] = Coreader[S, Reader[S, A]] // = (S, S => A)

  type CompositeStore[S1, S2, A] = (S1, (S2, S2 => (S1 => A)))

  def composeAdjComonad[S1, S2]: Comonad[CompositeStore[S1, S2, ?]] = {
    implicit val functorInstance = new Functor[({type λ[α] = (S1, (S2, α))})#λ] {
      override def map[A, B](fa: (S1, (S2, A)))(f: A => B) = {
        val (s1, (s2, a)) = fa
        (s1, (s2, f(a)))
      }
    }

    coreaderReader[S2].compose[(S1, ?), S1 => ?](coreaderReader[S1]).comonad
  }

  type Lens[S,A] = Kleisli[Store[S,?], A, A]

  object Lens {
    def fromGetSet[S,A](get: A => S, set: (S,A) => A): Lens[S,A] =
      Kleisli[Store[S,?], A, A](a => (get(a), (s => set(s,a))))

    def fromCompositeAdj[S1,S2,A](f: A => CompositeStore[S1,S2,A]): Lens[S1,A] =
      Kleisli[Store[S1,?], A, A]{ a =>
        val (s1, (s2, fs2)) = f(a)
        (s1, fs2(s2))
      }
  }
}

