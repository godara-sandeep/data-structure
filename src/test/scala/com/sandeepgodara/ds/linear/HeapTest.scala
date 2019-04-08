package com.sandeepgodara.ds.linear

import org.scalatest.{FlatSpec, Matchers}

class HeapTest extends FlatSpec with Matchers {

  "+" should "add elements in max Heap" in {
    val heap = new Heap[Int](11)
    heap + 3
    heap + 5
    heap + 12
    heap + 18
    Console println heap
    heap.size shouldEqual 4
  }

  "poll" should "return the root of heap and remove from heap" in {
    val heap = new Heap[Int](11)
    heap + 18
    heap + 3
    heap + 5
    heap + 12
    heap.poll() shouldEqual 18
    Console println heap
    heap.poll()
    heap.poll()
    heap.poll() shouldEqual 3
  }

  "sort" should "sort the list in asc order" in {
    val heap = new Heap[Int](11)
    heap + 10
    heap + 2
    heap + 6
    heap + 4
    heap + 3
    Console println heap
    heap.sort()
    heap.toString shouldEqual "[2,3,4,6,10]"
  }

  "buildHeap" should "make heap property" in {
    val heap = new Heap[Int](11)
    heap + 10
    heap + 2
    heap + 6
    heap + 4
    heap + 3
    Console println heap
    heap.sort()
    heap.toString shouldEqual "[2,3,4,6,10]"
    Console println heap
    heap.buildHeap()
    Array(10, 6, 4, 3, 2).forall(_ == heap.poll()) shouldEqual true
  }


  "-" should "Remove element from heap" in {
    val heap = new Heap[Int](11)
    heap + 15
    heap + 25
    heap + 12
    heap + 11
    heap + 46
    heap - 12
    heap - 46
    heap.size shouldEqual 3
    Array(25, 15, 11).forall(_ == heap.poll()) shouldEqual true
  }

}
