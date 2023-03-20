package lab03

object Solutions extends App

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
      case Nil() => right
      case Cons(h, t) => Cons(h, append(t, right))
