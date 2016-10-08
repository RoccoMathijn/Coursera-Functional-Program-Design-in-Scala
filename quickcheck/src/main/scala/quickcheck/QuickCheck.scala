package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert two elements") = {
    val m = insert(2, insert(1, empty))
    findMin(m) == 1
  }

  property("insert four elements") = {
    val m = insert(5, insert(2, insert(1, insert(4, insert(3, empty)))))
    findMin(m) == 1
  }

  property("delete min of empty heap") = {
    val m = insert(2, empty)
    deleteMin(m) == empty
  }

  property("delete min of non empty heap") = {
    val m = insert(5, insert(6, insert(2, insert(3, empty))))
    findMin(deleteMin(m)) == 3
  }

  property("melding") = {
    val m = insert(2, insert(3, empty))
    val n = insert (4, insert(1, empty))
    val meldHeap = meld(m, n)
    findMin(meldHeap) == findMin(m) || findMin(meldHeap) == findMin(n)
  }

  property("melding2") = {
    val m = insert(2, insert(3, empty))
    val n = insert(1, empty)
    val meldHeap = meld(m, n)
    findMin(meldHeap) == findMin(m) || findMin(meldHeap) == findMin(n)
  }


  property("ordening") = {
    val m: H = insert(5, insert(4, insert(3, insert(9, empty))))

    @tailrec
    def loop(heap: H, acc: List[Int]): List[Int] = {
        if (isEmpty(heap)) acc
        else  loop(deleteMin(heap),  acc :+ findMin(heap))
    }
    loop(m, List.empty) == loop(m, List.empty).sorted
  }

  property("deleter") = {
    val list = List(1,2,3,4,5,6,7,8,9)
    val h = inserter(empty, list)
    val g = deleteMin(h)
    findMin(g) == 2
  }

  def deleter(h: H): H = {
    if(isEmpty(h)) h
    else deleter(deleteMin(h))
  }

  def inserter(h: H, list: List[Int]): H = {
    if (list.isEmpty) h
    else inserter(insert(list.head, h), list.tail)
  }

}
