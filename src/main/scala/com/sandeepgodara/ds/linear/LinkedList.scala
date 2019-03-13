package com.sandeepgodara.ds.linear

class LinkedList[A] {

  class Node(var data: A) {
    var next: Node = _
  }

  var head: Node = _ //Initialize default value [null] with scala default value indicator =_

  def +(element: A): Unit = {
    val node = new Node(element)
    if (head == null) {
      head = node
    }
    else {
      findTailNode(this.head).next = node
    }
  }

  def +(element: A, position: Int): Unit = {
    def previousPos = position - 1

    def find(pos: Int = 0, node: Node): Node = {
      if (node == null) {
        throw new IndexOutOfBoundsException(s"Index out of bounds: $position")
      }
      if (pos == previousPos) {
        node
      }
      else {
        find(pos + 1, node.next)
      }
    }

    if (position == 0) {
      val node = new Node(element)
      node.next = head
      head = node
    }
    else {
      val previousNode = find(node = head)
      val node = new Node(element)
      node.next = previousNode.next
      previousNode.next = node
    }
  }

  def -(element: A): A = {

    def remove(previous: Node, next: Node): Unit = {
      if (next == null) {
        throw new NoSuchElementException(s"Element not found $element")
      }
      if (next.data == element) {
        previous.next = next.next
      }
      else {
        remove(next, next.next)
      }
    }

    if (head == null) {
      throw new NoSuchElementException(s"Element not found $element")
    }
    else if (head.data == element) {
      head = head.next
    }
    else {
      remove(head, head.next)
    }
    element
  }

  def indexOf(element: A): Int = {
    def findIndex(node: Node, i: Int = 0): Int = {
      if (node == null) {
        throw new NoSuchElementException(s"Element doesn't exist : $element")
      }
      if (node.data == element) {
        i
      }
      else
        findIndex(node.next, i + 1)
    }

    findIndex(head)
  }

  def contains(element: A): Boolean = { // Check if list contains the element
    def has(node: Node): Boolean = {
      if (node == null) false
      else if (node.data == element) true
      else has(node.next)
    }

    has(head)
  }

  def tail: LinkedList[A] = {
    //Return list with head removed
    if (head == null) {
      throw new IllegalStateException("Empty List")
    }
    else if (head.next == null) {
      head = null
      this
    }
    else {
      val newHead = head.next
      head.next = null
      head = newHead
      this
    }
  }

  def size: Int = {
    def calcSize(node: Node, s: Int = 0): Int = {
      if (node == null) s
      else calcSize(node.next, s + 1)
    }

    calcSize(head)
  }

  private def findTailNode(node: Node): Node = {
    if (node.next == null) node
    else findTailNode(node.next)
  }

  override def toString: String = {

    def buildString(node: Node, strings: List[String] = Nil): String = {
      if (node != null) {
        buildString(node.next, s"${node.data}" :: strings)
      }
      else strings.reverse.mkString(",")
    }

    buildString(head)
  }

  def hasLoop: Boolean = {
    if (head == null || head.next == null) {
      false
    }
    else {
      def checkLoop(slow: Node, fast: Node): Boolean = {
        if (slow == fast.next) {
          true
        }
        else {
          if (fast.next == null) {
            false
          }
          else {
            val onStepAhead = fast.next
            val twoStepAhead = if (onStepAhead.next == null) onStepAhead else onStepAhead.next
            checkLoop(slow.next, twoStepAhead)
          }
        }
      }
      checkLoop(head, head)
    }
  }

  def mid: A = {
    def move(slow: Node, fast:Node): A = {
      if(fast.next == null || fast.next.next == null){
        slow.data
      }
      else{
        move(slow.next, fast.next.next)
      }
    }
    if(head == null){
      throw new IllegalStateException("Empty List")
    }
    move(head, head)
  }

  def fac(n: Int): Int={
    if(n == 1) n
    else
    n * fac(n-1)
  }


}
