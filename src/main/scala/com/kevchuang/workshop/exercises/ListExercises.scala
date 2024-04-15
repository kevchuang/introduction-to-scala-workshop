package com.kevchuang.workshop.exercises

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
  override def isEmpty: Boolean                               = ???
  override def length: Int                                    = ???
  override def reverse: MyList[Nothing]                       = ???
  override def ++[B >: Nothing](that: MyList[B]): MyList[B]   = ???
  override def filter(f: Nothing => Boolean): MyList[Nothing] = ???
  override def map[B](f: Nothing => B): MyList[B]             = ???
  override def flatMap[B](f: Nothing => MyList[B]): MyList[B] = ???
}

case class ::[+A](head: A, tail: MyList[A]) extends MyList[A] {
  override def isEmpty: Boolean                         = ???
  override def length: Int                              = ???
  override def reverse: MyList[A]                       = ???
  override def ++[B >: A](that: MyList[B]): MyList[B]   = ???
  override def filter(f: A => Boolean): MyList[A]       = ???
  override def map[B](f: A => B): MyList[B]             = ???
  override def flatMap[B](f: A => MyList[B]): MyList[B] = ???
}

object MyList {
  def apply[A](as: A*): MyList[A] = ???
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
  def addOne(l: MyList[Int]): MyList[Int] = ???

  @main def testAddOne(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = MyList(2, 3, 4)

    assert(addOne(list) == expected)
  }

  /** Exercise 5 - Implement multiplyByTwo that will multiply by 2 each element
    * of the list
    */
  def multiplyByTwo(l: MyList[Int]): MyList[Int] = ???

  @main def testMultiplyByTwo(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = MyList(2, 4, 6)

    assert(multiplyByTwo(list) == expected)
  }

  /** Exercise 6 - Implement map function
    */
  @main def testMap(): Unit = {
    val list                  = MyList(1, 2, 3)
    val expectedPlusOne       = MyList(2, 3, 4)
    val expectedMultiplyByTwo = MyList(2, 4, 6)

    assert(list.map(_ + 1) == expectedPlusOne)
    assert(list.map(_ * 2) == expectedMultiplyByTwo)
  }

  /** Exercise 7 - Implement filter function
    */
  @main def testFilter(): Unit = {
    val list     = MyList(1, 2, 3, 4)
    val expected = MyList(2, 4)

    assert(list.filter(_ % 2 == 0) == expected)
  }

  /** Exercise 8 - Implement flatMap function
    */
  @main def testFlatMap(): Unit = {
    val list     = MyList(1, 2, 3)
    val expected = MyList(1, 2, 2, 3, 3, 4)

    assert(list.flatMap(a => MyList(a, a + 1)) == expected)
  }
}
