/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

import scala.io.Source
import scala.util.Random
import java.util.zip.GZIPInputStream
import scala.collection.mutable.HashEntry
import scala.collection.mutable.DefaultEntry
import scala.collection.mutable.HashMap

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
    val rootChildren = tree.root.values
    assert(rootChildren.size == 1, "Root node should have 1 child.")
//    val a = rootChildren('a').asInstanceOf[Leaf[alphabet.Internal]]
//    assert(a.edgeStart == 0)
//    assert(a.length(lastPhase) == 1)
  }

  val seed = new Random().nextInt()
  it should "build from a large, random string using the seed " + seed in {
    val stream = new Random(seed).alphanumeric
    val tree = SuffixTree(alphabet) += stream.take(1000000).toIndexedSeq
    tree.terminate()
  }

  it should "use memory?" in {
    import System.out
    import org.openjdk.jol.info.{ClassLayout,GraphLayout}
    import org.openjdk.jol.util.VMSupport

    val tree = SuffixTree(alphabet)
    val is = getClass.getClassLoader.getResourceAsStream("dm6.fa.gz")
    assume(is != null, "Test data file not available.")
    try {
      val stream = Source.fromInputStream(new GZIPInputStream(is))
      for (line <- stream.getLines().take(10000) if !line.startsWith(">"))
        tree += line.toUpperCase
    }
    finally {
      is.close()
    }
    tree.terminate()

    System.gc()
    out.println( VMSupport.vmDetails() )
    out.println( GraphLayout.parseInstance(tree).toFootprint() )
    out.println( ClassLayout.parseClass(classOf[InternalNode[_]]).toPrintable(tree) )
  }
}
