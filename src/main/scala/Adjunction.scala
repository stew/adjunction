package adjunction

import cats._


/**
 * An Adjunction is a relationship between two functors:
 *   - `F`, the "left-adjoint"
 *   - `G`, the "right-adjoint"
 * 
 * Which states the for every:
 * 
 *    `left-adjunct` (⎣⎦): F[A] => B, there exists a
 *     (you can think of ⎣ as looking like the "L" in "Left")

 *    `right-ajunct` (⎡⎤): B => G[A]
 *     (you can think of ⎡ as looking like an "r" in "right")
 * 
 *          F[A] ⊣ G[B]
 *            |      ^
 *      ⎣⎦    |      |    ⎡⎤    
 *            V      |
 *            B      A
 * 
 *
 * This gives rise to two functions:
 *
 *   - `left`:     A => ⎣⎦ => G[B]
 * 
 *   - `right`: F[A] => ⎡⎤ => B
 * 
 */

abstract class Adjunction[F[_], G[_]] { self =>

  /**
   * Given a left-adjunct and an A, produces a G[B].
   */
  def left[A,B](a: A)(f: F[A] => B): G[B]

  /**
   *  Given a right-adjunct and a F[A], produces a B.
   */
  def right[A,B](fa: F[A])(f: A => G[B]): B

  /**
   * Wrap an A into a G[F[A] context.
   */ 
  def unit[A](a: A): G[F[A]] =
    left(a)(identity)

  /**
   * Extract an A from a F[G[A].
   */
  def counit[A](fga: F[G[A]]): A =
    right(fga)(identity)


  /**
   * given any two adjoint functors, we can create a monad of their composite
   */
  def monad(implicit G: Functor[G]): Monad[λ[α => G[F[α]]]] =
    new Monad[λ[α => G[F[α]]]] {
      override def pure[A](a: A) = unit(a)

      override def map[A,B](gfa: G[F[A]])(f: A => B): G[F[B]] =
        flatMap(gfa)(f andThen unit)

      override def flatMap[A,B](gfa: G[F[A]])(f: A => G[F[B]]): G[F[B]] =
        G.map(gfa)(right(_)(f))
    }

  /**
   * given any two adjoint functors, we can create a comonad of their composite
   */
  def comonad(implicit F: Functor[F]): Comonad[λ[α => F[G[α]]]] =
    new Comonad[λ[α => F[G[α]]]] {
      override def extract[A](fga: F[G[A]]): A = counit(fga)

      override def map[A,B](fga: F[G[A]])(f: A => B): F[G[B]] =
        coflatMap(fga)(f compose counit)

      override def coflatMap[A,B](fga: F[G[A]])(f: F[G[A]] => B): F[G[B]] =
        F.map(fga)(left(_)(f))
    }

  /**
   * we can compose one adjunction with another
   */
  def compose[H[_], I[_]](HI: H ⊣ I): Adjunction[λ[α => H[F[α]]], λ[α => G[I[α]]]] =
    new Adjunction[λ[α => H[F[α]]], λ[α => G[I[α]]]] {
      override def left[A,B](a: A)(f: H[F[A]] => B): G[I[B]] =
        self.left(a)(HI.left(_)(f))

      override def right[A,B](hfa: H[F[A]])(f: A => G[I[B]]): B =
        HI.right(hfa)(self.right(_)(f))
    }
}
object Adjunction {

  type Reader[S,R] = S => R

  type Coreader[S,R] = (S,R)

  /**
   * 
   */
  def coreaderReader[S]: (Coreader[S,?] ⊣ Reader[S,?]) = 
    new (Coreader[S,?] ⊣ Reader[S,?]) {
      def left[A, B](a: A)(f: Coreader[S,A] => B): Reader[S,B] =
        s => f((s,a))

      def right[A,B](sa: Coreader[S,A])(f: A => Reader[S,B]): B =
        f(sa._2)(sa._1)
    }
}
