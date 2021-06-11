package vn.khanhpdt.dsalgs.datastructures.linkedlists

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class SkiplistSortedSetSuite extends AnyFunSuite {

  test("Add new items") {
    val set = new SkiplistSortedSet[Int]

    Random.shuffle[Int, Seq[Int]](Range.inclusive(1, 10).toList).foreach { i =>
      printSet(set, i)
    }
  }

  private def printSet(set: SkiplistSortedSet[Int], i: Int): Unit = {
    println(s"New item: $i")
    set.add(i)
    println(set)
    println("---------------------------")
  }

  test("Add new items in increasing order") {
    val set = new SkiplistSortedSet[Int]

    Range.inclusive(1, 10).foreach { i =>
      printSet(set, i)
    }
  }

  test("Add new items in decreasing order") {
    val set = new SkiplistSortedSet[Int]

    Range.inclusive(1, 10).reverse.foreach { i =>
      printSet(set, i)
    }
  }

}
