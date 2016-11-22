package adjunction

import cats._


/**
 * An Adjunction is a relationship between two functors:
 *   - `F`, the "left-adjoint"
 *   - `G`, the "right-adjoint"
 * 
 * customarily expressed as `F ⊣ G`.
 * 
 * Which states the for every:
 * 
 *    `left-adjunct` (named `⎣⎦`): F[A] => B, there exists a

 *    `right-adjunct` (named `⎡⎤`): B => G[A]

 *     (for mnemonics, you can think of ⎣ as looking like the "l" in "left-adjunct",
 *     and ⎡ as looking like an "r" in "right-adjunct")
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
 *   - `left`:  ⎡⎤ => ⎣⎦ 

 *   - `right`: ⎣⎦ => ⎡⎤ 
 * 
 *
 * ⎡ | 0x23A1 | (C-x 8 ENTER) LEFT SQUARE BRACKET UPPER CORNER

 * ⎤ | 0x23A4 | (C-x 8 ENTER) RIGHT SQUARE BRACKET UPPER CORNER

 * ⎣ | 0x23A3 | (C-x 8 ENTER) LEFT SQUARE BRACKET LOWER CORNER

 * ⎦ | 0x23A6 | (C-x 8 ENTER) RIGHT SQUARE BRACKET LOWER CORNER

 */

abstract class Adjunction[F[_], G[_]] { self =>
  // left-adjunct (⎣ looks like an l)
  type ⎣⎦[A,B] = A => G[B]

  // right-adjunct (⎡ looks like an r)
  type ⎡⎤[A,B] = F[A] => B

  /**
   *  Given a right-adjunct produce the left-adjunct
   */
  def left[A,B](f: A ⎡⎤ B): A ⎣⎦ B

  /**
   * Given a left-adjunct produce the right-adjunct. 
   */
  def right[A,B](fa: A ⎣⎦ B): A ⎡⎤ B

  /**
   * Wrap an A into a G[F[A] context. This is the `pure` operation of
   * the monad for G[F[?]] which arises from this adjunction.
   */ 
  def unit[A]: A => G[F[A]] = left(identity[F[A]])

  /**
   * Extract an A from a F[G[A]. This is the `copure` operation of the 
   *  comonad for F[G[A]] that which from this adjunction
   */
  def counit[A]: F[G[A]] => A = right(identity[G[A]])

  /**
   * given any two adjoint functors, we can create a monad of their composite
   */
  def monad(implicit G: Functor[G]): Monad[λ[α => G[F[α]]]] =
    new Monad[λ[α => G[F[α]]]] {

      override def pure[A](a: A) = unit(a)

      override def map[A,B](gfa: G[F[A]])(f: A => B): G[F[B]] =
        flatMap(gfa)(f andThen unit)

      override def flatMap[A,B](gfa: G[F[A]])(f: A => G[F[B]]): G[F[B]] =
        G.map(gfa)(right(f))

      /**
       * this is not actually tail recursive, but I'm not sure that's possible
       */
      override def tailRecM[A, B](a: A)(f: A => G[F[scala.Either[A, B]]]): G[F[B]] =
        flatMap(f(a)) {
          case scala.Left(a) => tailRecM(a)(f)
          case scala.Right(b) => unit(b)
        }
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
        F.map(fga)(left(f))
    }

  /**
   * we can compose one adjunction with another
   */
  def compose[H[_], I[_]](HI: H ⊣ I): Adjunction[λ[α => H[F[α]]], λ[α => G[I[α]]]] =
    new Adjunction[λ[α => H[F[α]]], λ[α => G[I[α]]]] {

      override def left[A, B](f: H[F[A]] => B): A => G[I[B]] = self.left(HI.left(f))

      override def right[A, B](f: A => G[I[B]]): H[F[A]] => B = {
        val xxx: F[A] => I[B] = self.right(f)
        val yyy: H[F[A]] => B = HI.right(xxx)
        yyy
      }
    }
}

object Adjunction {

  type Reader[S,R] = S => R

  type Coreader[S,R] = (S,R)

  def idid[A]: (Id ⊣ Id) =
    new (Id ⊣ Id) {
      override def left[A,B](f: A => B): A => B = f
      override def right[A,B](f: A => B): A => B = f
    }
  
  def coreaderReader[S]: (Coreader[S,?] ⊣ Reader[S,?]) = 
    new (Coreader[S,?] ⊣ Reader[S,?]) {
      override def left[A, B](f: Coreader[S,A] => B): A => Reader[S,B] =
        a => s => f((s,a))

      override def right[A,B](f: A => Reader[S,B]): Coreader[S,A] => B =
        sa => f(sa._2)(sa._1)
    }
}
