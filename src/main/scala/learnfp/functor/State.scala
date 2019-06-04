package learnfp.functor

case class State[S, A](run:S => (S, A)) {
  def exec(s:S):S = run(s)._1
  def eval(s:S):A = run(s)._2
}

object State {
  def put[S](ns:S):State[S, Unit] = State[S,Unit](  _ => (ns,Unit) )
  def get[S]:State[S, S] = State[S,S]((x:S) => (x,x))
  def modify[S](fx:S => S):State[S, Unit] = State[S,Unit](s => (fx(s),Unit))
}

object StateInstance {
  implicit def stateInstance[S] = new Functor[({type E[X] = State[S, X]})#E] {
    override def fmap[A, B](a: State[S, A])(fx: A => B): State[S, B] = State[S,B](a.run.andThen(x=> (x._1,fx(x._2))))
  }

  implicit def toFunctorOps[S, A, State[S, A]](f:State[S, A])(implicit functor:Functor[({type E[X] = State[S, X]})#E]) =
    new FunctorOps[A, ({type E[X] = State[S, X]})#E](f)

  class StateFxOps[A, R](fx:A => R) {
    def `<$>`[S](a:State[S, A]):State[S, R] = a fmap fx
  }

  implicit def toStateFxOps[A, R](fx:A => R) = new StateFxOps(fx)
}
