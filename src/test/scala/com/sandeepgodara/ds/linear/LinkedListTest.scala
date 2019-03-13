package com.sandeepgodara.ds.linear

import org.scalatest.{FlatSpec, Matchers}

class LinkedListTest extends FlatSpec with Matchers {



  "+" should "add element to list" in{
    val list = new LinkedList[Int]()
    list + 1
    list.size shouldEqual 1
  }

  "-" should "remove element from list" in{
    val list = new LinkedList[Int]()
    list + 2
    list.size shouldEqual 1
    list - 2
    list.size shouldEqual 0
  }

  "contains" should "tell whether element exists in list"in{
    val list = new LinkedList[Int]()
    list + 5
    list.contains(7) shouldEqual false
    list.contains(5) shouldEqual true
    list + 8
    list + 9
    list.contains(8) shouldEqual true
    list.contains(10) shouldEqual false
  }

  "+(e,pos)" should "Add element at specified position"in{
    val list = new LinkedList[Int]()
    list + 1
    list + (2, 0)
    list.indexOf(2) shouldEqual 0
    list + 3
    list + 4
    list + 5
    list + (6, 4)
    list.indexOf(6) shouldEqual 4
    list.indexOf(5) shouldEqual 5
  }

  "tail" should "return a the list with all elements minus head" in{
    val list = new LinkedList[Int]()
    list + 1
    list + 2
    list + 3
    list + 4
    list + 5
    list.tail.size shouldEqual 4
  }

  "hasLoop" should "tell if there's a loop in the list" in{
    val list = new LinkedList[Int]()
    list + 1
    list.head.next = list.head
    list.hasLoop shouldEqual true
  }

  "hasLoop" should "tell if another case loop" in{
    val list = new LinkedList[Int]()
    list + 1
    list + 5
    list.head.next.next = list.head
    list.hasLoop shouldEqual true
  }

  "hasLoop" should "tell if another case 3 loop" in{
    val list = new LinkedList[Int]()
    list + 1
    list + 5
    list + 9
    list + 7

    list.head.next.next.next = list.head.next
    list.hasLoop shouldEqual true
  }

  "hasLoop" should "tell if another case 4 loop" in{
    val list = new LinkedList[Int]()
    list + 1
    list + 5
    list + 9
    list + 7
    list + 7
    list + 7
    list + 77
    list + 45
    list.head.next.next.next.next.next.next.next = list.head.next.next
    list.hasLoop shouldEqual true
  }

  "mid" should "tell the middle element in the list" in{
    val list = new LinkedList[Int]()
    list + 2
    list + 3
    list + 5
    list + 9
    list + 10
    list.mid shouldEqual 5
  }




}
