package learnfp.typeclass

trait Show[A] {
  def show(x:A):String
}

object Printer {
  def show[A](x:A)(implicit showInstance:Show[A]):String = {
    showInstance.show(x)
  }
}

object ShowInstances {
  implicit val intInstance:Show[Int] = (x: Int) => s"$x"

  implicit val doubleInstance:Show[Double] = new Show[Double] {
    override def show(x: Double): String = s"$x"
  }

  implicit def listInstance[T](implicit xShow:Show[T]):Show[List[T]] = new Show[List[T]] {
    override def show(xs:List[T]): String = xs.map(xShow.show).mkString("[",", ","]")
  }
}