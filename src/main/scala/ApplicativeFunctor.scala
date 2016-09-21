object ApplicativeFunctor {

  import scala.language.higherKinds

  trait Applicative[F[_]] {
    def point[A](a: A): F[A]

    def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(point(f))
  }

  def identityLaw[F[_], A](fa: F[A])(implicit F: Applicative[F]): Boolean =
    F.ap(fa)(F.point((a: A) => a)) == fa

  def homomorphismLaw[F[_], A, B](f: A => B, a: A)(implicit F: Applicative[F]): Boolean =
    F.ap(F.point(a))(F.point(f)) == F.point(f(a))

  def interchangeLaw[F[_], A, B](f: F[A => B], a: A)(implicit F: Applicative[F]): Boolean =
    F.ap(F.point(a))(f) == F.ap(f)(F.point((g: A => B) => g(a)))

  implicit object OptionApplicative extends Applicative[Option] {
    override def point[A](a: A): Option[A] = Some(a)

    override def ap[A, B](fa: Option[A])(f: Option[(A) => B]): Option[B] = f match {
      case Some(g) => fa match {
        case Some(a) => Some(g(a))
        case None => None
      }
      case None => None
    }
  }

}