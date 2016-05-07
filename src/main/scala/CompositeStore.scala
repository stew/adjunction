package adjunction

import cats._

object CompositeStore {

  import Adjunction._

  /**
    * Coreader ⊣ Reader gives rise to a comonad when composed as
    * `Writer[S, Reader[S, α]] => α`, and the Comonad is for `(s, s => α)` which
    * looks just like the Store Comonad!
    **/

  type Store[S, A] = Coreader[S, Reader[S, A]] // = (S, S => A)

  type Storeful[S, A, B] = Store[S, A] => B

  type ComposedStore[S1, S2, A] = (S1, (S2, S2 => (S1 => A)))

  def composeAdjComonad[S1, S2]: Comonad[ComposedStore[S1, S2, ?]] = {
    implicit val functorInstance = new Functor[({type λ[α] = (S1, (S2, α))})#λ] {
      override def map[A, B](fa: (S1, (S2, A)))(f: A => B) = {
        val (s1, (s2, a)) = fa
        (s1, (s2, f(a)))
      }
    }

    coreaderReader[S2].compose[(S1, ?), S1 => ?](coreaderReader[S1]).comonad
  }
}
