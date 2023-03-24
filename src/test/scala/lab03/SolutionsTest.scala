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

  // Task 1 tests
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

  // Task 2 tests
  import Option.*

  @Test
  def testMax(): Unit =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  // Task 3 tests
  import Person.*

  val students: List[Person] = Cons(Student("Enrico", 2000), Cons(Student("Giovanni", 1999), Cons(Student("Lorenzo", 2001), Nil())))
  val teachers: List[Person] = Cons(Teacher("Viroli", "pps"), Cons(Teacher("Ricci", "cdp"), Cons(Teacher("Mirri", "web"), Nil())))
  val persons: List[Person] = Cons(Student("Enrico", 2000), Cons(Teacher("Viroli", "pps"), Nil()))

  @Test
  def testCourses(): Unit =
    assertEquals(Nil(), courses(students))
    assertEquals(Cons("pps", Cons("cdp", Cons("web", Nil()))), courses(teachers))
    assertEquals(Cons("pps", Nil()), courses(persons))

  // Task 4 tests
  val lst: List[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test
  def testFoldLeft(): Unit =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(32, foldLeft(lst)(0)(_ + _ * 2))
    assertEquals(17, foldLeft(lst)(1)(_ + _))
    assertEquals(5, foldLeft[Int, Int](Nil())(5)(_ + _))
    assertEquals(16.1, foldLeft(lst)(0.1)(_ + _), 0)

  @Test
  def testFoldRight(): Unit =
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(16, foldRight(lst)(0)(_ + _))
    assertEquals(-15, foldRight(lst)(1)(_ * 2 - _))
    assertEquals(2, foldRight(lst)(2)(_ * 0 + _))
    assertEquals(5, foldRight[Int, Int](Nil())(5)(_ + _))
    assertEquals(-7.5, foldRight(lst)(0.5)(_ - _), 0)

  // Task 5 tests

  @Test
  def testStreamDrop(): Unit = {
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    val s2 = Stream.take(Stream.iterate(1)(_*2))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))
    assertEquals(Cons(16, Cons(32, Cons(64, Cons(128, Cons(256, Cons(512, Nil())))))), Stream.toList(Stream.drop(s2)(4)))
  }

  @Test
  def tesConstant(): Unit = {
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))
  }

  @Test
  def testScanLeft(): Unit = {
    val s: Stream[Int] = Stream.iterate(1)(_ + 1)
    assertEquals(Cons(1, Cons(2, Cons(4, Nil()))), Stream.toList(Stream.scanLeft(Stream.take(s)(3))(1)(_ + _)))
  }

  val fibs: Stream[Int] = Stream.cons(0, Stream.scanLeft(fibs)(1)(_ + _))
  @Test
  def testFibs(): Unit = {
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))
  }