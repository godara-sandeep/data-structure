package com.sandeepgodara.ds

import org.scalatest.{FlatSpec, Matchers}

class AVLTreeSpec extends FlatSpec with Matchers{

  "+1" should "add elements in AVL tree self balancing tree"in{
    val tree = new AVLTree[Int]()
    tree + 15
    tree + 20
    tree + 10
    tree + 17
    tree + 18
    tree.height shouldEqual 3
    tree.printLevelOrder()
  }
  "+2" should "add elements in AVL tree self balancing tree"in{
    val tree = new AVLTree[Int]()
    tree + 15
    tree + 20
    tree + 12
    tree + 14
    tree + 13
    tree.height shouldEqual 3
    tree.printLevelOrder()
  }
  "+3" should "add elements in AVL tree self balancing tree"in{
    val tree = new AVLTree[Int]()
    tree + 25
    tree + 20
    tree + 30
    tree + 32
    tree + 35
    tree.height shouldEqual 3
    tree.printLevelOrder()
  }

}
