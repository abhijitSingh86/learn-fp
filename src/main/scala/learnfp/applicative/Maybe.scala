package learnfp.applicative

import learnfp.functor.Maybe.Maybe
import learnfp.functor.{MaybeInstance => MaybeFunctorInstance}

import learnfp.functor.Maybe.{Maybe, Just, Nothing}


object MaybeInstance {
  import MaybeFunctorInstance._
  import learnfp.functor.FunctorOps._
  implicit val maybeApplicativeInstance = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = Just(a)
    override def <*>[A, R](fx: Maybe[A => R])(a: Maybe[A]): Maybe[R] = a match {
      case Nothing() => Nothing[R]()
      case Just(v) => fx.map(x=>x(v))
    }
  }
}
