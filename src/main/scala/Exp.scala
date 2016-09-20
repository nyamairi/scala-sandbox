sealed abstract class Exp

case class Add(lhs: Exp, rhs: Exp) extends Exp

case class Sub(lhs: Exp, rhs: Exp) extends Exp

case class Mul(lhs: Exp, rhs: Exp) extends Exp

case class Div(lhs: Exp, rhs: Exp) extends Exp

case class Lit(value: Int) extends Exp
