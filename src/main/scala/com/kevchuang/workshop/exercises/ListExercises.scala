package com.kevchuang.workshop.exercises

import scala.annotation.tailrec

sealed abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def ::[B >: A](elem: B): MyList[B] = new ::(elem, this)
  def length: Int
  def reverse: MyList[A]
  def ++[B >: A](that: MyList[B]): MyList[B]
  def filter(f: A => Boolean): MyList[A]
  def map[B](f: A => B): MyList[B]
  def flatMap[B](f: A => MyList[B]): MyList[B]
}

case object Nil extends MyList[Nothing] {
  override def head: Nothing                                  = throw new NoSuchElementException
  override def tail: MyList[Nothing]                          = throw new NoSuchElementException
  override def isEmpty: Boolean                               = true
  override def length: Int                                    = 0
  override def reverse: MyList[Nothing]                       = this
  override def ++[B >: Nothing](that: MyList[B]): MyList[B]   = that
  override def filter(f: Nothing => Boolean): MyList[Nothing] = this
  override def map[B](f: Nothing => B): MyList[B]             = this
  override def flatMap[B](f: Nothing => MyList[B]): MyList[B] = this
}

case class ::[+A](head: A, tail: MyList[A]) extends MyList[A] {
  override def isEmpty: Boolean = false
  override def length: Int = {
    @tailrec
    def loop(remaining: MyList[A], acc: Int): Int =
      remaining match
        case Nil    => acc
        case h :: t => loop(t, acc + 1)

    loop(this, 0)
  }

  override def reverse: MyList[A] = {
    @tailrec
    def loop(remaining: MyList[A], acc: MyList[A]): MyList[A] =
      remaining match
        case Nil    => acc
        case h :: t => loop(t, h :: acc)
    loop(this, Nil)
  }

  override def ++[B >: A](that: MyList[B]): MyList[B] = {
    @tailrec
    def loop(
        left: MyList[B],
        full: MyList[B]
    ): MyList[B] =
      left match {
        case Nil    => full
        case h :: t => loop(t, h :: full)
      }

    loop(this.reverse, that)
  }

  override def filter(f: A => Boolean): MyList[A] = {
    @tailrec
    def loop(remaining: MyList[A], acc: MyList[A]): MyList[A] =
      remaining match
        case Nil            => acc.reverse
        case h :: t if f(h) => loop(t, h :: acc)
        case h :: t         => loop(t, acc)
    loop(this, Nil)
  }

  override def map[B](f: A => B): MyList[B] = {
    @tailrec
    def loop(remaining: MyList[A], acc: MyList[B]): MyList[B] =
      remaining match
        case Nil    => acc.reverse
        case h :: t => loop(t, f(h) :: acc)
    loop(this, Nil)
  }

  override def flatMap[B](f: A => MyList[B]): MyList[B] = {
    @tailrec
    def loop(remaining: MyList[A], acc: MyList[B]): MyList[B] =
      remaining match
        case Nil    => acc
        case h :: t => loop(t, acc ++ f(h))

    loop(this, Nil)
  }
}

object MyList {
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else as.head :: apply(as.tail*)

}

object ListExercises {

  /** Exercise 1 - Implement MyList.apply method
    */
  @main def testApply(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = 1 :: 2 :: 3 :: Nil

    assert(list == expected)
  }

  /** Exercise 2 - Implement length method, it should return the length of the
    * list
    */
  @main def testLength(): Unit = {
    val list = MyList(1, 2, 3)

    assert(list.length == 3)
  }

  /** Exercise 3 - Implement length method using tail recursion
    */
  @main def testBiggerLength(): Unit = {
    val args = for (i <- 0 to 100000) yield i
    val list = args.foldLeft(Nil: MyList[Int])((acc, v) => v :: acc)

    assert(list.length == args.length)
  }

  /** Exercise 4 - Implement addOne method that will add plus 1 to each element
    * of the list
    */
  def addOne(l: MyList[Int]): MyList[Int] =
    l match
      case Nil    => Nil
      case h :: t => h + 1 :: addOne(t)

  @main def testAddOne(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = MyList(2, 3, 4)

    assert(addOne(list) == expected)
  }

  /** Exercise 5 - Implement multiplyByTwo that will multiply by 2 each element
    * of the list
    */
  def multiplyByTwo(l: MyList[Int]): MyList[Int] =
    l match
      case Nil    => Nil
      case h :: t => h * 2 :: multiplyByTwo(t)

  @main def testMultiplyByTwo(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = MyList(2, 4, 6)

    assert(multiplyByTwo(list) == expected)
  }

  /** Exercise 6 - Implement filter function
    */
  @main def testFilter(): Unit = {
    val list     = MyList(1, 2, 3, 4)
    val expected = MyList(2, 4)

    assert(list.filter(_ % 2 == 0) == expected)
  }

  /** Exercise 7 - Implement map function
    */
  @main def testMap(): Unit = {
    val list                  = MyList(1, 2, 3)
    val expectedPlusOne       = MyList(2, 3, 4)
    val expectedMultiplyByTwo = MyList(2, 4, 6)

    assert(list.map(_ + 1) == expectedPlusOne)
    assert(list.map(_ * 2) == expectedMultiplyByTwo)
  }

  /** Exercise 8 - Implement flatMap function
    */
  @main def testFlatMap(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = MyList(1, 2, 2, 3, 3, 4)

    assert(list.flatMap(a => MyList(a, a + 1)) == expected)
  }
}
