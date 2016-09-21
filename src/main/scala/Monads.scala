object Monads {

  import scala.language.higherKinds

  trait Monad[F[_]] {
    def point[A](a: A): F[A]

    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

    def ap[A, B](fa: F[A])(f: F[A => B])(implicit F: Monad[F]): F[B] =
      F.bind(f)((g: A => B) => F.bind(fa)((a: A) => F.point(g(a))))
  }

  def rightIdentityLaw[F[_], A](a: F[A])(implicit F: Monad[F]): Boolean =
    F.bind(a)(F.point(_)) == a

  def leftIdentityLaw[F[_], A, B](a: A, f: A => F[B])(implicit F: Monad[F]): Boolean =
    F.bind(F.point(a))(f) == f(a)

  def associativeLaw[F[_], A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit F: Monad[F]): Boolean =
    F.bind(F.bind(fa)(f))(g) == F.bind(fa)((a: A) => F.bind(f(a))(g))

  implicit object OptionMonad extends Monad[Option] {
    override def point[A](a: A): Option[A] = Some(a)

    override def bind[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }
  }

}
