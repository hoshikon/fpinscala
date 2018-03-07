package fpinscala.datastructures

import fpinscala.SimpleBooleanTest

object TreeTest extends App with SimpleBooleanTest {
  import Tree._

  def run = {
    val tree1 = Branch(Leaf(2), Branch(Leaf(5), Leaf(8)))

    val sizeTest = size(tree1) == 5
    println(sizeTest + ": size")

    val maxTest = maximum(tree1) == 8
    println(maxTest + ": maximum")

    val depthTest = depth(tree1) == 2
    println(depthTest + ": depth")

    val tree2 = Branch(Leaf("2"), Branch(Leaf("5"), Leaf("8")))
    val mapTest = map(tree1)(_.toString) == tree2
    println(mapTest + ": map")

    val sizeWithFoldTest = sizeWithFold(tree1) == size(tree1)
    println(sizeWithFoldTest + ": size with fold")

    val maxWithFoldTest = maximumWithFold(tree1) == maximum(tree1)
    println(maxWithFoldTest + ": maximum with fold")

    val depthWithFoldTest = depthWithFold(tree1) == depth(tree1)
    println(depthWithFoldTest + ": depth with fold")

    val mapWithFoldTest = mapWithFold(tree1)(_.toString) == map(tree1)(_.toString)
    println(mapWithFoldTest + ": map with fold")
  }
}