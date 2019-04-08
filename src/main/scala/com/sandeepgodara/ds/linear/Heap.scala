package com.sandeepgodara.ds.linear

import scala.annotation.tailrec
import scala.reflect.ClassTag

class Heap[A](capacity: Int)(implicit m: ClassTag[A], ordering: Ordering[A]) {
  val heap = new Array[A](capacity)
  var size = 0

  def +(e: A): Unit = {
    if (size == capacity) {
      throw new IllegalStateException("Heap is full! can't add more elements")
    }
    heap(size) = e
    shiftUp(size)
    size += 1
  }

  def poll(): A = {
    val root = heap(0)
    heap(0) = heap(size - 1)
    heap(size - 1) = _
    size -= 1
    heapify(0, size)
    root
  }

  def -(element: A): Unit = {
    val index = heap.indexOf(element)
    if (index == -1) {
      throw new NoSuchElementException(s"Element: $element doesn't exists in the collection.")
    }
    heap(index) = heap(size - 1)
    size -= 1
    if (ordering.gt(heap(index), heap(findParentIndex(index)))) {
      shiftUp(index)
    }
    else {
      heapify(index, size)
    }
  }

  def contains(element: A): Boolean = {
    if (size == 0) {
      false
    } else if (ordering.gt(element, heap(0))) {
      false
    } else {
      heap.contains(element)
    }
  }

  @tailrec
  private def shiftUp(index: Int): Unit = {
    val parentIndex = findParentIndex(index)

    if (ordering.gt(heap(index), heap(parentIndex))) {
      swap(index, parentIndex)
      shiftUp(parentIndex)
    }
  }

  @tailrec
  private def heapify(index: Int, size: Int): Unit = {
    if (index < size - 1) {
      val leftIndex = left(index)
      val rightIndex = right(index)
      val swapIdx = swapIndex(index, leftIndex, rightIndex, size)
      if (swapIdx != index) {
        swap(swapIdx, index)
        heapify(swapIdx, size)
      }
    }
  }

  def sort(): Unit = {
    @tailrec
    def doSort(pointer: Int): Unit = {
      if (pointer > 0) {
        swap(0, pointer)
        heapify(0, pointer)
        doSort(pointer - 1)
      }
    }

    doSort(size - 1)
  }


  private def swapIndex(parentIndex: Int, leftIndex: Int, rightIndex: Int, size: Int): Int = {
    val index = if (leftIndex <= size - 1) {
      if (ordering.gt(heap(leftIndex), heap(parentIndex))) {
        leftIndex
      }
      else parentIndex
    }
    else {
      parentIndex
    }

    if (rightIndex <= size - 1) {
      if (ordering.gt(heap(rightIndex), heap(index)))
        rightIndex
      else
        index
    }
    else index
  }

  //@TODO Test pending, need to take array in constructor.
  def buildHeap(): Unit = {
    val lastTreeIndex = leafStartIndex - 1

    @tailrec
    def balance(i: Int): Unit = {
      if (i >= 0) {
        heapify(i, size)
        balance(i - 1)
      }
    }

    balance(lastTreeIndex)
  }

  private def left(index: Int) = (2 * index) + 1

  private def right(index: Int) = (2 * index) + 2

  private def swap(index1: Int, index2: Int): Unit = {
    val tmp = heap(index1)
    heap(index1) = heap(index2)
    heap(index2) = tmp
  }

  private def leafStartIndex = {
    Math.floor(size / 2).toInt
  }

  private def findParentIndex(index: Int) = {
    Math.max((index / 2) + (index % 2) - 1, 0)
  }

  override def toString: String = {
    s"[${Range(0, size).map(heap(_)).mkString(",")}]"
  }

}
