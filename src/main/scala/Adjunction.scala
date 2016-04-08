package adjunction

import cats._

/** A composition of dunces^Wfunctors */
trait ◦[F[_], G[_]] { type λ[A] = F[G[A]] }

abstract class Adjunction[F[_], G[_]] { self =>

  def left[A,B](a: A)(f: F[A] => B): G[B]
  def right[A,B](fa: F[A])(f: A => G[B]): B

  def unit[A](a: A): G[F[A]] =
    left(a)(identity)

  def counit[A](fga: F[G[A]]): A =
    right(fga)(identity)

  /**
   * given any two adjoint functors, we can create a monad of their composite
   */
  def monad(implicit G: Functor[G]): Monad[(G ◦ F)#λ] =
    new Monad[(G ◦ F)#λ] {
      def pure[A](a: A) = unit(a)
      def flatMap[A,B](gfa: G[F[A]])(f: A => G[F[B]]): G[F[B]] =
      G.map(gfa)(right(_)(f))
    }

  /**
   * given any two adjoint functors, we can create a comonad of their composite
   */
  def comonad(implicit F: Functor[F]): Comonad[(F ◦ G)#λ] =
    new Comonad[(F ◦ G)#λ] {
      def extract[A](fga: F[G[A]]): A = counit(fga)

      def map[A,B](fga: F[G[A]])(f: A => B): F[G[B]] =
        coflatMap(fga)(a => f(counit(a)))

      def coflatMap[A,B](fga: F[G[A]])(f: F[G[A]] => B): F[G[B]] =
        F.map(fga)(left(_)(f))
    }

  /**
   * we can compose one adjunction with another
   */
  def compose[H[_], I[_]](HI: H ⊣ I): (H ◦ F)#λ ⊣ (G ◦ I)#λ = {
    new ((H ◦ F)#λ ⊣ (G ◦ I)#λ) {
      def left[A,B](a: A)(f: H[F[A]] => B): G[I[B]] =
        self.left(a)(HI.left(_)(f))

      def right[A,B](hfa: H[F[A]])(f: A => G[I[B]]): B =
        HI.right(hfa)(self.right(_)(f))
    }
  }
}
