package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(elem, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("findMin (1)") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin (2)") = forAll { (a1: A, a2: A) =>
    val h = insert(a1, insert(a2, empty))
    findMin(h) == math.min(a1, a2)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("deleteMin (1)") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("deleteMin (2)") = forAll { (a1: A, a2: A) =>
    val h = insert(a2, insert(a1, empty))
    findMin(deleteMin(h)) == math.max(a1, a2)
  }

  property("deleteMin (3)") = forAll { (a1: A, a2: A, a3: A) =>
    val h = insert(a3, insert(a2, insert(a1, empty)))
    findMin(deleteMin(deleteMin(h))) == math.max(a3, math.max(a1, a2))
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
    */
  property("sorted") = forAll { (h: H) =>
    def isSorted(last: A, h: H): Boolean = {
      isEmpty(h) || {
        val min = findMin(h)
        last <= min && isSorted(min, deleteMin(h))
      }
    }

    isSorted(findMin(h), deleteMin(h))
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("meld (1)") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

  property("meld (2)") = forAll { a: A =>
    val h = insert(a, empty)
    isEmpty(deleteMin(meld(h, empty)))
  }


}
