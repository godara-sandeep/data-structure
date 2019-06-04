package com.sandeepgodara.ds

class AVLTree[A](implicit ordering: Ordering[A]) {

  private var root: Node = _

  class Node(var data: A) {
    var left: Node = _
    var right: Node = _
    var height: Int = 1

    override def toString: String = s"{data: $data, height: $height}"
  }

  def +(element: A): Unit = {
    root = insert(element, root)
  }

  private def insert(element: A, node: Node): Node = {
    if (node == null) new Node(element)
    else {
      if (ordering.gt(element, node.data)) {
        node.right = insert(element, node.right)
      }
      else {
        node.left = insert(element, node.left)
      }
      balance(node)
    }
  }

  /**
    * This function compute the balance factor
    * (difference between height of left subtree and height of right subtree)
    * If the absolute difference is greater then 1 balance the skewed (left or right) subtree
    * @param node tree node
    * @return
    */
  private def balance(node: Node): Node = {
    node.height = calcHeight(node)
    val factor = height(node.left) - height(node.right)
    if (factor > 1) {
      balanceLeft(node)
    }
    else if (factor < -1) {
      balanceRight(node)
    }
    else {
      node.height = calcHeight(node)
      node
    }
  }

  /**
    * Compute height of node as 1 + Max(leftSubTreeHeight, rightSubTreeHeight)
    * @param node tree node
    * @return
    */
  private def calcHeight(node: Node) = 1 + Math.max(height(node.left), height(node.right))

  private def height(node: Node) = if (node == null) 0 else node.height

  /**
    * Tree is skewed towards right
    * @param node tree node
    * @return
    */
  private def balanceRight(node: Node): Node = {
    if (node.right.right == null) {
      >><<(node)
    }
    else {
      <<(node)
    }
  }

  /**
    * Tree is skewed towards left
    * @param node tree node
    * @return
    */
  private def balanceLeft(node: Node): Node = {
    if (node.left.left == null) {
      <<>>(node)
    }
    else {
      >>(node)
    }
  }

  def height: Int = root.height

  /**
    * Rotate left
    * @param node tree node
    * @return
    */
  private def <<(node: Node): Node = {
    val root = node.right
    node.right = root.left
    root.left = node
    root.left.height -= 2
    root
  }

  /**
    * Rotate right
    * @param node tree node
    * @return
    */
  private def >>(node: Node): Node = {
    val root = node.left
    node.left = root.right
    root.right = node
    root.right.height -= 2
    root
  }

  /**
    * Rotate left right
    * @param node tree node
    * @return
    */
  private def <<>>(node: Node): Node = {
    val mid = node.left.right
    node.left.right = mid.left
    mid.left = node.left
    node.left = mid
    node.left.height += 1
    node.left.left.height -= 1
    >>(node)
  }

  /**
    * Rotate right left
    * @param node tree node
    * @return
    */
  private def >><<(node: Node): Node = {
    val mid = node.right.left
    node.right.left = mid.right
    mid.right = node.right
    node.right = mid
    node.right.height += 1
    node.right.right.height -= 1
    <<(node)
  }

  /**
    * Function to print all nodes level by level
    */
  def printLevelOrder(): Unit = {
    printLevel(List(root))
  }

  private def printLevel(nodes: List[Node]): Unit = nodes match {
    case Nil =>
    case _ =>
      val next = nodes.filter(_ != null).flatMap(node => {
        Console print s"$node, "
        List(node.left, node.right)
      })
      Console println ""
      printLevel(next)
  }
}
