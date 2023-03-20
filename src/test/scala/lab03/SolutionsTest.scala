package lab03
import lab03.List.*
import org.junit.Test
import org.junit.Assert.*

class SolutionsTest:

  /**
   * type List[A]
   * operations:
   *  drop[A]: List[A] x int -> List[A]
   *  append[A]: List[A] x List[A] -> List[A]
   *
   * axioms:
   *  drop(Nil(), i) = Nil()
   *  drop(Cons(h, t), 0) = Cons(h, t)
   *  drop(Cons(h, t), i) = drop(t, i - 1)
   *  append(Nil(), Cons(h, t)) = Cons(h, t)
   *  append(Cons(h, t), Cons(h1, t1)) = Cons(h, append(t, Cons(h1, t1)))
   */

  val list: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val empty: List[Int] = Nil()

  @Test
  def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(list, 1))
    assertEquals(Cons(30, Nil()), drop(list, 2))
    assertEquals(empty, drop(list, -10))
    assertEquals(empty, drop(list, 5))

  @Test
  def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(list, tail))
    assertEquals(empty, append(empty, empty))
    assertEquals(list, append(empty, list))
    assertEquals(list, append(list, empty))
