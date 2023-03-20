package lab03

import org.junit.Test
import org.junit.Assert.*
import Solutions.*

class SolutionsTest:
  import List.*
  /**
   * type List[A]
   * operations:
   *  drop[A]: List[A] x int -> List[A]
   *  append[A]: List[A] x List[A] -> List[A]
   *  flatMap: List[A] x (A -> List[B]) -> List[B]
   *
   * axioms:
   *  drop(Nil(), i) = Nil()
   *  drop(Cons(h, t), 0) = Cons(h, t)
   *  drop(Cons(h, t), i) = drop(t, i - 1)
   *  append(Nil(), Cons(h, t)) = Cons(h, t)
   *  append(Cons(h, t), Cons(h1, t1)) = Cons(h, append(t, Cons(h1, t1)))
   *  flatMap(Nil())(f) = Nil()
   *  flatMap(Cons(h,t))(v => f(v)) = append(f(h), flatMap(t)(f))
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

  @Test
  def testFlatMap(): Unit =
    assertEquals(Cons(11, Nil()), flatMap(Cons(10, Nil()))(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(list)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(list)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test
  def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(list)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(list)(_+""))

  @Test
  def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(list)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(list)(_ != 20))
