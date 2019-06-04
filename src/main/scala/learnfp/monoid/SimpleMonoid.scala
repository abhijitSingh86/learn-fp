package learnfp.monoid

object SimpleMonoid {
  case class Sum(value:Int)
  implicit val sumMonoidInstance:Monoid[Sum] = new Monoid[Sum] {
    override def mzero: Sum = Sum(0)
    override def mappend(lhs: Sum, rhs: Sum): Sum = Sum(lhs.value+rhs.value)
  }

  case class Product(value:Int)
  implicit val productMonoidInstance:Monoid[Product] = new Monoid[Product] {
    override def mzero: Product = Product(0)
    override def mappend(lhs: Product, rhs: Product): Product = {
      (lhs.value,rhs.value ) match {
        case (0,0) => lhs
        case (0,x) => rhs
        case (x,0) => lhs
        case (x,y) => Product(x*y)
      }
    }
  }
}
