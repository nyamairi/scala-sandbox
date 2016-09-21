object Additives {

  trait Additive[A] {
    def plus(lhs: A, rhs: A): A

    def zero: A
  }

  implicit object StringAdditive extends Additive[String] {
    override def plus(lhs: String, rhs: String): String = lhs + rhs

    override def zero: String = ""
  }

  implicit object IntAdditive extends Additive[Int] {
    override def plus(lhs: Int, rhs: Int): Int = lhs + rhs

    override def zero: Int = 0
  }

  case class Point(x: Int, y: Int)

  implicit object PointAdditive extends Additive[Point] {
    override def plus(lhs: Point, rhs: Point): Point = Point(lhs.x + rhs.x, lhs.y + rhs.y)

    override def zero: Point = Point(0, 0)
  }

  case class Rational(num: Int, den: Int)

  object Rational {

    implicit object RationalAdditive extends Additive[Rational] {
      override def plus(lhs: Rational, rhs: Rational): Rational = {
        if (lhs == zero) {
          rhs
        } else if (rhs == zero) {
          lhs
        } else {
          Rational(lhs.num * rhs.den + rhs.num * lhs.den, lhs.den * rhs.den)
        }
      }

      override def zero: Rational = Rational(0, 0)
    }

  }

  def sum[A](list: List[A])(implicit m: Additive[A]) = list.foldLeft(m.zero)((x, y) => m.plus(x, y))
}