package adjunction

import cats._

object ComposedState {
  import Adjunction._
  /**
   * Coreader ⊣ Reader gives rise to a monad when composed as `α =>
   * Reader[S, Writer[S, α]], and the Monad is for `s => (s, α)` which
   * looks just like the state Monad!
   */

  type State[S,A] = Reader[S, Coreader[S, A]] // = S => (S,A)

  type Stateful[S, A, B] = A => State[S,B]

  type ComposedState[S1,S2,A] = S1 => (S2 => (S2, (S1, A)))

  def composeAdjMonad[S1,S2]: Monad[ComposedState[S1,S2, ?]] = {
//  Functor#compose seems to be broken in cats because it conflicts with Invariant#compose
//    val f1: Functor[S1 => ?] = cats.std.function.function1Covariant
//    val f2: Functor[S2 => ?] = cats.std.function.function1Covariant
//    implicit val f3: Functor[({type λ[α] = S1 => (S2 => α)})#λ] = f1.compose(f2)
    implicit val f3 = new Functor[({type λ[α] = S1 => (S2 => α)})#λ] {
      override def map[A,B](fa: S1 => (S2 => A))(f: A => B) = s1 => s2 => f(fa(s1)(s2))
    }

    coreaderReader[S1].compose[(S2,?), S2 => ?](coreaderReader[S2]).monad
  }

  /**
   * combine two Stateful functions that perfom As such that a
   * structure of As can be traversed once doing both computations in
   * parallel, then combining their results with a gi
   */
  def twoInParallel[A,S1,S2,B,C,R](sf1: Stateful[S1,A,B],
                                   sf2: Stateful[S2,A,C],
                                   f: (B,C) => R): A => ComposedState[S1,S2,R] = 
    (a: A) => s1 => s2 => {
      val (ns2,c) = sf2(a)(s2)
      val (ns1,b) = sf1(a)(s1)
      (ns2, (ns1, f(b,c)))
    }


  /**
   * combine an Stateful[S1, A, B] with a Stateful[S2, B, C] so that
   * one traversal of As can compute a C, using both stateful serially
   * computations.
   */
  def feedOneAnother[A, S1, S2, B, C](sf1: Stateful[S1,A,B],
                                      sf2: Stateful[S2,B,C]):
      A => ComposedState[S1,S2,C] = a => s1 => s2 => {
    val (ns1,b) = sf1(a)(s1)
    val (ns2,c) = sf2(b)(s2)
    (ns2, (ns1, c))
  }

  /**
   * traverse an F full of As with a composite stateful computation
   */
  def traverseS2[F[_], S1, S2, A, C](fa: F[A])(s: A => ComposedState[S1, S2, C])(s1: S1, s2: S2)(implicit F: Traverse[F]): (S1, S2, F[C]) = {
    val st = F.traverse[ComposedState[S1,S2,?], A, C](fa)(s)(composeAdjMonad[S1,S2])
    val (ns2, (ns1, c)) = st(s1)(s2)
    (ns1, ns2, c)
  }
 }


