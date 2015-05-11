package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (x: Int, y: Int) =>
    val h = insert(x, insert(y, empty))
    findMin(h) == (if (x > y) y else x)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("empty") = forAll { x: Int =>
    val h = insert(x, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { (h: H) =>
    val l = toList(h)
    l == l.sorted
  }

  property("meld-min") = forAll { (x: Int, y: Int) =>
    val h1 = insert(x, empty)
    val h2 = insert(y, empty)
    findMin(meld(h1, h2)) == (if (x > y) y else x)
  }

  property("meld-sorted") = forAll { (x: H, y: H) =>
    val m = toList(meld(x, y))
    val v = (toList(x) ++ toList(y)).sorted
    m == v
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- if (i % 20 == 0) const(empty) else genNonEmptyHeap
  } yield h

  lazy val genHeapPair: Gen[(H, H)] = for {
    x <- arbitrary[H]
    y <- arbitrary[H]
  } yield (x, y)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def genNonEmptyHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  def toList(h: H): List[A] =
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
}
