package learnfp.typeclass

trait Eq[A] {
  def eq(lhs: A, rhs: A): Boolean
}

object Eq {
  def eq[A](lhs: A, rhs: A)(implicit eqt: Eq[A]) = eqt.eq(lhs, rhs)
}

class EqOps[A](lhs: A)(implicit eqt: Eq[A]) {
  def ====(rhs: A): Boolean = eqt.eq(lhs, rhs)
}

object EqOps {
  implicit def toEqOps[A](lhs: A)(implicit eqt: Eq[A]) = new EqOps(lhs)
}

object EqInstances {

  implicit val intEqInstance = new Eq[Int] {
    override def eq(lhs: Int, rhs: Int): Boolean = lhs == rhs
  }

  implicit val stringEqInstance = new Eq[String] {
    override def eq(lhs: String, rhs: String): Boolean = lhs.compareTo(rhs) == 0
  }

  implicit def listEqInstance[A](implicit eqt: Eq[A]) = new Eq[List[A]] {
    override def eq(lhs: List[A], rhs: List[A]): Boolean = {
      if (lhs.size == rhs.size)
        lhs.zip(rhs).map(x => eqt.eq(x._1, x._2)).foldLeft(true)(_ && _)
      else {
        false
      }
    }
  }

}
