package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("lI1|") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin: empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin: two elements") = forAll { (a: Int, b: Int) =>
    (a < b) ==> {
      val h = insert(b, insert(a, empty))
      findMin(h) == a
    }
  }

  property("insert: order") = forAll { (a: Int, b: Int) =>
    val h1 = insert(b, insert(a, empty))
    val h2 = insert(a, insert(b, empty))
    h1 == h2
  }

  property("deleteMin: with one element returns an empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("findMin & deleteMin: elements ordered") = forAll { (as: List[Int]) =>
    as.nonEmpty ==> {
      def popAll(h: H): List[Int] = {
        def popIter(h: H, iter: List[Int]): List[Int] = {
          if (isEmpty(h))
            iter
          else
            popIter(deleteMin(h), iter :+ findMin(h))
        }

        popIter(h, List())
      }

      val xs = as.sorted
      val ys = popAll(createHeap(xs))

      xs == ys
    }
  }

  property("meld min") = forAll { (as: List[Int], bs: List[Int]) =>
    (as.nonEmpty && bs.nonEmpty) ==> {
      val ah = createHeap(as)
      val bh = createHeap(bs)

      val h = meld(ah, bh)
      val inMin = math.min(as.min, bs.min)

      findMin(h) == inMin
    }
  }

  def createHeap(as: List[Int]): H = as match {
    case (_ :: _) => as.map(insert(_, empty)).reduce(meld)
    case _ => empty
  }
}