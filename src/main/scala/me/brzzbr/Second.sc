import scala.language.{higherKinds, reflectiveCalls}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// LazyFunctor = Yoneda
trait Yoneda[F[_], A] {
  def transform[B](f: A => B): F[B]

  def run: F[A] = transform(identity)
}

// лемма Йонеды:
// 1. есть функция
// def map[B](f: A => B): F[B] (1)
// следовательно, мы всегда можем получить F[A], передав identity-функцию
// 2. Обратное так же верно
// для значения F[A] для каждого функтора F и типа A существует функция (1)
// ВЫВОД: для каждого функтора F и типа A между Yoneda[F,A] и F[A] существует изоморфизм
// ВАЖНО: для того, чтобы работать с Yoneda, F должен быть функтором

type Iso[A, B] = (A => B, B => A)

object Yoneda {

  def yoFIso[F[_], A](implicit F: Functor[F]): Iso[Yoneda[F, A], F[A]] = (fromYo, fa => toYo(fa))

  def fromYo[F[_], A](yo: Yoneda[F, A]): F[A] = yo transform identity

  def toYo[F[_], A](fa: F[A])(implicit F: Functor[F]): Yoneda[F, A] = new Yoneda[F, A] {
    def transform[B](f: A => B) = F.map(fa)(f)
  }

  // Койонеда так же ожидаемо является функтором
  implicit def yoFunctor[F[_]]: Functor[({type YF[A] = Yoneda[F, A]})#YF] = new Functor[({type YF[A] = Yoneda[F, A]})#YF] {
    def map[A, B](fa: Yoneda[F, A])(f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
      def transform[C](g: B => C): F[C] = fa.transform(f andThen g)
    }
  }

  implicit class YonedaOps[F[_], A](yo: Yoneda[F, A]) {
    type YO[B] = Yoneda[F, B]
    def map[C](f: A => C)(implicit F: Functor[YO]): Yoneda[F, C] = F.map(yo)(f)
  }
}

// койонеда
// Есть value F[A], для КАЖДОГО F и А мы може
// деконструировать его в value типа F[B] и
// функцию B => A, по крайней мере для некоторых B.
// Даже если F не является функтором
// Т.о. мы можем "отложить" restriction на [F: Functor] "на потом"
// когда нам понадобится "вытащить" F[A] из CoYoneda[F[_], A]

trait CoYoneda[F[_], A] { self =>
  type I // т.н. pivot-type
  def f: I => A
  def fi: F[I]

  import CoYoneda._
  // restriction отложен до момента вычисления F[A]
  def run(implicit F: Functor[F]): F[A] = fromCoYo(self)
}

object CoYoneda {
  // Лемма Йонеды гласит, что между CoYoneda[F[_], A] и F[A] существует изоморфизм
  def coyoFIso[F[_], A](implicit F: Functor[F]): Iso[CoYoneda[F, A], F[A]] = (coyo => fromCoYo(coyo), toCoYo)

  def fromCoYo[F[_], A](coyo: CoYoneda[F, A])(implicit F: Functor[F]): F[A] = F.map(coyo.fi)(coyo.f)

  def toCoYo[F[_], A](fa: F[A]): CoYoneda[F, A] = new CoYoneda[F, A] {
    type I = A
    val f = identity
    val fi = fa
  }

  // Койонеда так же ожидаемо является функтором
  implicit def coyoFunctor[F[_]]: Functor[({type CF[A] = CoYoneda[F, A]})#CF] = new Functor[({type CF[A] = CoYoneda[F, A]})#CF] {
    def map[A, B](fa: CoYoneda[F, A])(g: A => B): CoYoneda[F, B] = new CoYoneda[F, B] {
      type I = fa.I
      def f = fa.f andThen g
      def fi = fa.fi
    }
  }

  implicit class CoYonedaOps[F[_], A](coyo: CoYoneda[F, A]) {
    def map[B](f: A => B): CoYoneda[F, B] = coyoFunctor.map(coyo)(f)
  }
}

import CoYoneda._

case class Person[A](a: A)
val personCoyo0: CoYoneda[Person, Int] = toCoYo(Person(42))
val personCoyo1: CoYoneda[Person, Int] = personCoyo0.map(_ + 1).map(_ + 2).map(_ + 3)
// всё так же Lazy

// для конечных вычислений Person должен быть Functor-ом
implicit val personFunctor: Functor[Person] = new Functor[Person] {
  def map[A, B](fa: Person[A])(f: (A) => B): Person[B] = Person(f(fa.a))
}

// вычисляем результат
val person: Person[Int] = personCoyo1.run
