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