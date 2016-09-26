object Monoids {

  trait Monoid[F] {
    def append(lhs: F, rhs: F): F

    def zero: F
  }

  def leftIdentityLaw[F](a: F)(implicit F: Monoid[F]): Boolean = a == F.append(F.zero, a)

  def rightIdentityLaw[F](a: F)(implicit F: Monoid[F]): Boolean = a == F.append(a, F.zero)

  def associativeLaw[F](a: F, b: F, c: F)(implicit F: Monoid[F]): Boolean = {
    F.append(F.append(a, b), c) == F.append(a, F.append(b, c))
  }

  implicit object OptionIntMonoid extends Monoid[Option[Int]] {
    override def append(lhs: Option[Int], rhs: Option[Int]): Option[Int] = (lhs, rhs) match {
      case (None, None) => None
      case (Some(v), None) => Some(v)
      case (None, Some(v)) => Some(v)
      case (Some(v1), Some(v2)) => Some(v1 + v2)
    }

    override def zero: Option[Int] = None
  }

}
