package lab03

object Solutions extends App:

  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:
    def fold[A](opt: Option[A])(df: A)(f: A => A): A = (opt, df) match
      case (Some(a), _) => f(a)
      case (_, d) => d

  enum List[A]:
    case Cons(head: A, tail: List[A])
    case Nil()

  object List:
    // Task 1
    // a)
    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (l, 0) => l
      case (Cons(_, t), i) => drop(t, i - 1)
    // b)
    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right
    // c)
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()
    // d)
    def map[A,B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(x => Cons(mapper(x), Nil()))
    // e)
    def filter[A](l: List[A])(pred: A => Boolean): List[A] = flatMap(l)(x => pred(x) match
      case true => Cons(x, Nil())
      case false => Nil())

    // Task 2
    import Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => Some(fold(max(t))(h)(_.max(h)))
      case Nil() => None()

    // Task 4
    def foldLeft[A, B](l: List[A])(d: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(d, h))(f)
      case Nil() => d

    def foldRight[A, B](l: List[A])(d: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight(t)(d)(f))
      case Nil() => d


  // Task 3
  import Person.*
  import List.*

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  def courses(l: List[Person]): List[String] = flatMap(l)(_ match
    case Teacher(_, c) => Cons(c, Nil())
    case _ => Nil())

  // Streams
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, tail), n) if n > 0 => drop(tail())(n - 1)
      case _ => stream

    // Task 7
    def scanLeft[A](stream: => Stream[A])(n: A)(f: (A, A) => A): Stream[A] = stream match
      case Cons(head, tail) => cons(n, scanLeft(tail())(f(n, head()))(f))
      case _ => Empty()


    // Task 6
    def constant[A](const: A): Stream[A] = iterate(const)(_ => const)