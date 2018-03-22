
import scala.language.{higherKinds, reflectiveCalls}

def f(a: Int): String = ???
def g(a: String): Option[Int] = ???
def h(a: Option[Int]): List[String] = ???

// три прохода по миллиону итераций... беда
def foo = (0 to 10000000).map(f).map(g).map(h)

// так как List является функтором
// fmap (g . f) = fmap g . fmap f

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// то, почему бы не абстрагироваться от F[_] и не запилить LazyFunctor?
trait LazyFunctor[F[_], A] {
  def transformation[B](f: A => B): F[B]
  def run: F[A] = transformation(identity)
}

// LazyFunctor является функтором... неожиданно=)
implicit def lazyFunctorFunctor[F[_]]: Functor[({type LF[A] = LazyFunctor[F, A]})#LF] = ???

// и из любого функтора можно сделать LazyFunctor
def toLazyFunctor[F[_], A](fa: F[A])(implicit F: Functor[F]): LazyFunctor[F, A] = ???

// обратное так же гарантировано
def fromLazyFunctor[F[_], A](lf: LazyFunctor[F, A]): F[A] = ???

object LazyFunctor {
  // для удобства использования map на LazyFunctor
  implicit class LazyFunctorOps[F[_], A](lf: LazyFunctor[F, A]) {
    type LF[B] = LazyFunctor[F, B]
    def map[C](f: A => C)(implicit F: Functor[LF]): LazyFunctor[F, C] = F.map(lf)(f)
  }
}

// нужен для демонстрации LazyFunctor[Option, A]
implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  def map[A, B](fa: Option[A])(f: A => B) = fa match {
    case Some(a) => Some(f(a))
    case _       => None
  }
}

val lazyOption: LazyFunctor[Option, Int] = toLazyFunctor(Some(42))
val transformed: LazyFunctor[Option, Int] = lazyOption.map(_ + 1).map(_ + 2)
// вычисления над lazyOption еще не произведены

// применяем все трансформации за раз и получаем Some(45)
val option: Option[Int] = fromLazyFunctor(transformed)
