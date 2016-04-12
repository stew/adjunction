package object adjunction {
  import cats.Functor

  type ⊣[F[_], G[_]] = Adjunction[F, G]
}
