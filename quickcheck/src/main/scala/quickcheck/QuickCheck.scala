package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == List(a,b).min
  }

  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }
  
  property("gen1") = forAll { (h: H) =>
  	val m = if (isEmpty(h)) 0 else findMin(h)
  	findMin(insert(m, h))==m
  }

  property("min4") = forAll { a: H =>
    def sortedConent(a:H, acc:List[Int]): List[Int] = {
      if (isEmpty(a)) acc else sortedConent(deleteMin(a), findMin(a)::acc)
    }  
    val res = sortedConent(a,List())
//    println(a)
//    println(res)
//    println(res.sorted)
    res.reverse == res.sorted     
  }
  
  property("min5") = forAll { (a: H, b:H) =>
    try {
    	val h = findMin(meld(a,b))
    	h == findMin(a) || h == findMin(b)
    } catch {
      case e: NoSuchElementException => true
    }
  }
  
  property("min6") = forAll { (a: H, b:H) =>
    try {
    	val h = findMin(deleteMin(meld(a,b)))
    	h == findMin(a) || h == findMin(b) || h == findMin(deleteMin(a)) || h == findMin(deleteMin(b))
    } catch {
      case e: NoSuchElementException => true
    }
  }

  property("min6") = forAll { (a: H) =>
    try {
    	val h = findMin(meld(a,a))
    	h == findMin(a)
    } catch {
      case e: NoSuchElementException => true
    }
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    c <- oneOf(const(empty), genHeap)
  } yield insert(x,c)
    
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
