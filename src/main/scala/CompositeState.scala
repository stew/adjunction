package adjunction

import shapeless._
import cats._

object ComposedState {
  type State[S,A] = S => (S, A)
  type Stateful[S, A, B] = A => State[S,B]

  sealed trait StateComputation[S, A, B]

  final case class ||:[S,A,B,SS,BB, C, T <: StateComputation[SS,A,BB]](run: Stateful[S,A,B], f: (B, BB) => C) extends StateComputation[(S,SS), A, C]

  final case class |>:[S,A,B,SS,C,T <: StateComputation[SS,C,A]](run: Stateful[S,A,B]) extends StateComputation[(S, SS), C, B]

  sealed trait PSNil extends StateComputation[HNil, Any, HNil]

  def writerReader[S]: Adjunction[(S,?), S => ?] =
    new Adjunction[(S,?), S => ?] {
      def left[A, B](a: A)(f: ((S, A)) => B): S => B = s => f((s,a))
      def right[A,B](sa: (S,A))(f: A => S => B): B = f(sa._2)(sa._1)
    }

  type ComposedState[S1,S2,A] = S1 => (S2 => (S2, (S1, A)))

  def composeAdjMonad[S1,S2]: Monad[ComposedState[S1,S2, ?]] = {
//  Functor#compose seems to be broken in cats because it conflicts with Invariant#compose
//    val f1: Functor[S1 => ?] = cats.std.function.function1Covariant
//    val f2: Functor[S2 => ?] = cats.std.function.function1Covariant
//    implicit val f3: Functor[({type λ[α] = S1 => (S2 => α)})#λ] = f1.compose(f2)
    implicit val f3 = new Functor[({type λ[α] = S1 => (S2 => α)})#λ] {
      override def map[A,B](fa: S1 => (S2 => A))(f: A => B) = s1 => s2 => f(fa(s1)(s2))
    }

    (writerReader[S1].compose[(S2,?), S2 => ?](writerReader[S2])).monad
  }

  /**
   * combine two A => S => (S,B) functions that perfom stateful
   * computation on As such that a structure of As can be traversed
   * once doing both computations.
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
   * combine an A => S1 => (S1,B) with a B => S2 => (S2, B) so that
   * one traversal of As can compute a C, using both stateful
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


