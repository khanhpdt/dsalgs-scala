package vn.khanhpdt.dsalgs.datastructures.linkedlists

import scala.Ordering.Implicits._
import scala.collection.mutable.ArrayBuffer

/**
 * <a href="https://en.wikipedia.org/wiki/Skip_list">Skip list - Wikipedia</a>
 *
 * @tparam T type of items in the list
 */
class SkiplistSortedSet[T: Ordering] {

  // this stores the list of the first nodes at each level
  // items in this list represent the levels from top to bottom: top level -> ... -> bottom level
  private var sentinel: SkipListNode[T] = _

  // start from 0. -1 means empty.
  private var height: Int = -1

  def add(item: T): Unit = {
    if (Option(sentinel).isEmpty) {
      // workaround to set the sentinel node
      // we need to set a key to the sentinel node and there is no other way to instantiate the key
      sentinel = SkipListNode[T](item, ArrayBuffer.empty)
    }

    // this makes skiplist a probabilistic data structure
    val nodeHeight = pickRandomHeight()

    // to stop when a node with same value exists, b/c this is a set.
    var duplicatedFound = false

    // find previous nodes at all levels
    var h = Math.min(nodeHeight, height)
    val previousNodes = new Array[SkipListNode[T]](h + 1)
    var cursor = sentinel
    while (h >= 0 && !duplicatedFound) {
      while (h < cursor.next.size && cursor.next(h).key <= item && !duplicatedFound) {
        if (cursor.next(h).key == item) {
          duplicatedFound = true
        } else {
          // b/c item > node, item must be on the right of the node.
          // move right
          cursor = cursor.next(h)
        }
      }

      if (!duplicatedFound) {
        // if we get here, we know that node > item and item must be larger that the node's previous node.
        // the item will be inserted in between the node and its previous.
        previousNodes(h) = cursor
      }

      // move down
      h -= 1
    }

    if (!duplicatedFound) {
      val newNode = SkipListNode[T](item, ArrayBuffer.empty)

      // increase the overall height
      while (height < nodeHeight) {
        height += 1
        sentinel.next.addOne(newNode)
      }

      previousNodes.zipWithIndex.foreach {
        // h is height and we go bottom up
        case (prevNode, h) =>
          // if previous node has next at height h
          if (h < prevNode.next.size) {
            newNode.next.addOne(prevNode.next(h))
            prevNode.next(h) = newNode
          }

          // if the previous node is the last node at the height h
          if (prevNode.next.size <= h) {
            prevNode.next.addOne(newNode)
          }
      }
    }
  }

  private def pickRandomHeight(): Int = {
    var h = 0
    while (Math.random() <= 0.5) {
      h += 1
    }
    println(s"Pick height: ${h + 1}")
    h
  }

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(s"Height: ${height + 1}")

    sentinel.next.zipWithIndex.reverse.foreach {
      case (node, h) =>
        sb.append("\n")

        var n = node
        sb.append(n.key)

        while (n.next.size > h) {
          n = n.next(h)
          sb.append(s" - ${n.key}")
        }
    }

    sb.toString
  }

}

// next[i] is the next node of this node at level i
case class SkipListNode[T](key: T, next: ArrayBuffer[SkipListNode[T]])
