/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

import scala.util.Random

/** Test of the suffix tree. */
class SuffixTreeSpec extends UnitSpec {
  val alphabet = new NucleotideAlphabet()

  behavior of "A suffix tree"

  it should "build an empty tree" in {
    val tree = SuffixTree(alphabet)
    tree.terminate()
    assert(tree.size == 0, "Tree does not have expected size.")
  }

  it should "append properly" in {
    val tree = SuffixTree(alphabet) += "TAA"
    tree.terminate()
    assert(tree.size == 3, "Tree does not have expected size.")
  }

  it should "be multiply-appendable" in {
    val tree = SuffixTree(alphabet) += "TAA" += "TAA"
    tree.terminate()
    assert(tree.size == 6, "Tree does not have expected size.")
  }

  it should "not allow a tree to be terminated twice" in {
    val tree = SuffixTree(alphabet) += "TAA"
    tree.terminate()
    intercept[IllegalStateException] {
      tree.terminate()
    }
  }

  it should "build correctly" in {
    val tree = SuffixTree(alphabet) += "a"
    tree.terminate()

    val lastPhase = tree.size - 1
    val rootChildren = tree.root.children
    assert(rootChildren.size == 1, "Root node should have 1 child.")
    val a = rootChildren('a').asInstanceOf[Leaf[alphabet.Internal]]
    assert(a.edgeStart == 0)
    assert(a.length(lastPhase) == 1)
  }

  val seed = new Random().nextInt()
  it should "build from a large, random string using the seed " + seed in {
    val stream = new Random(seed).alphanumeric
    val tree = SuffixTree(alphabet) += stream.take(1000000).toIndexedSeq
    tree.terminate()
  }
}
