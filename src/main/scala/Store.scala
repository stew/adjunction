package adjunction

import cats._

object Store {
  // store is the comonad you get from Writer -| Reader
  type Store[S,A] = (S, S => A)

  type Lens[S,A] = S => Store[S,A]

  case class Name(first: String,
                  last: String)

  case class Person(name: Name,
                    age: Int)

  val ageLens: Lens[Person, Int] = person => (person, _.age)

  val nameLens: Lens[Person, Name] = person => (person, _.name)

  val firstLens: Lens[Name, String] = name => (name, _.first)

  type ComposedLens[S1, S2, A] = (S1, S1 => (S2, S2 => A))

  
//      def composeAdjComonad[S1,S2]: Comonad[ComposedLens[S1,S2, ?]] = {
//        implicit val composed: Functor[λ[α => (S1, (S2, α))]] = new Functor[λ[α => (S1, (S2, α))]] {
//          override def map[A,B](fa: (S1, (S2, A)))(f: A => B): (S1, (S2, B)) = (fa._1, (fa._2._1, f(fa._2._2)))
//        }
//  
//  // this is not the composition i'm looking for  
//  //        [error]  found   : cats.Comonad[[α](S1, (S2, S2 => (S1 => α)))]
//  //        [error]  required: cats.Comonad[[γ](S1, S1 => (S2, S2 => γ))]
//        (ComposedState.writerReader[S2].compose[(S1,?), S1 => ?](ComposedState.writerReader[S1])).comonad
//      }
}
