/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree;

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import com.typesafe.scalalogging.LazyLogging
import scala.util.control.Breaks
import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Suffix tree.
  * 
  * The implementation uses Ukkonen's algorithm, as described in Gusfield's ''Algorithms on Strings, Trees, and Sequences''.
  * Note that this book uses one-based indexing for phases and extensions, whereas this code uses zero-based indexing.
  * Comments in the code refer to the book's indexing method.
  * 
  * Following are the equalities between the variables used in Gusfield and those used here:
  * <table>
  *   <thead>
  *     <tr><th>Gusfield</th><th>code</th></tr>
  *   </thead>
  *   <tbody>
  *     <tr><td>''S''</td><td>`storedString`</td></tr>
  *     <tr><td>''S''[1]</td><td>`storedString(0)`</td></tr>
  *     <tr><td>''m''</td><td>`storedString.size`</td></tr>
  *     <tr><td>''i''</td><td>`phase` (starts at `0`)</td></tr>
  *     <tr><td>''j''-1</td><td>`extension` (starts at `0`)</td></tr>
  *     <tr><td>''j,,i,,''</td><td>`firstExplicitExtension` (starts at `0`)</td></tr>
  *     <tr><td>''v''</td><td>`newInternalNode`</td></tr>
  *   </tbody>
  * </table>
  * 
  * @see Dan Gusfield, ''Algorithms on Strings, Trees, and Sequences'', Cambridge University Press, 1997.
  * @constructor Create a suffix tree.
  * @tparam A The type of the string's alphabet.
  * @param alphabet Alphabet of the string symbols.
  * @throws IllegalArgumentException if the string is empty or contains a terminator symbol
  */
class SuffixTree[A <: Alphabet] private (alphabet: A)(implicit tag: ClassTag[A#Internal]) extends LazyLogging {
  private[this] val myBreaks = new Breaks
  import myBreaks.{break, breakable}

  require(alphabet != null, "No alphabet supplied.")

  /** Type of the internal representation of string symbols. */
  private[this] type I = A#Internal

  /** Type of the internal representation of string symbols. */
  private[this] type E = A#External

  /** Concatenation of all of the appended strings. */
  private[this] val storedString: Buffer[I] = ArrayBuffer()

  /** Child nodes, keyed by the internal node and the first element of their label. */
  private[this] val children = new TwoKeyOpenHashMap[InternalNode[I], I, Node[I]](100000, alphabet.size) {
    override def default(k1: InternalNode[I], k2: I) = null
  }

  /** Root node. */
  private[this] val _root = RootNode(children)

  /** The (zero-based) `extension` number at which to start the phase.
    * In Gusfield's notation, this would equal ''j,,i,,'' (allowing for the zero-basing) in phase ''i''+1.
    * It is one more than the `extension` number of the last explicit extension done.
    * 
    * This allows implementation of step 1 and the start of step 2 of "SPA" of §6.1.5 of Gusfield.
    */
  private[this] var startingExtension: Int = 0

  /** The end of ''S''[1..''i''], in Gusfield. */
  private[this] var leaf1: Leaf[I] = _

  /** (Zero-based) current phase number.
    * This is ''i'' in Gusfield's notation.
    * It is incremented just before the phase starts; so between phases, it represents the last phase.
    */
  private[this] var phase = -1

  /** Element to append in this phase.
    * 
    * If its value is the alphabet's terminal symbol,
    * the current string is either empty, or already terminated.
    * These conditions can be distinguished by whether `phase == 0`.
    */
  private[this] var element: I = alphabet.sentinel // dummy value for initialization

  /** Whether the string has been terminated. */
  private[this] var isTerminated = false

  /** (Zero-based) current extension number of the current phase. */
  private[this] var extension = 0

  /** Node at or above the end of the last-extended suffix
    * (''S''[''j''-1..''i''] in Gusfield's notation, if the current phase is ''i''+1 and the current extension is ''j'').
    */
  private[this] var lastEnd: InternalNode[I] = _root

  /** Characters below `lastEnd` where the end of the last-extended suffix is.
    * This identifies γ in SEA step 1 of §6.1.3 of Gusfield.
    */
  private[this] var lastEndOffset: Int = 0

  /** New internal node created in the previous extension. `null` if none was created.
    * This is ''v'' in Gusfield, stored in order to create a suffix link sourcing from it.
    */
  private[this] var newInternalNode: InternalNode[I] = _


  /** Append the given string to the tree's current string.
    * 
    * @param string A sequence of characters to append to the tree.
    * @return This object.
    * @throws IllegalArgumentException if the string contains a terminator symbol
    */
  def append(string: Traversable[alphabet.External]): this.type = {
    if (isTerminated)
      throw new IllegalStateException("The current string has already been terminated.")

    if (string != null && !string.isEmpty)
      string.foreach { character =>
        element = alphabet.convert(character)
        require(element != alphabet.sentinel,
          "Disallowed terminal symbol found at position " + (phase + 1 /* because it hasn't been incremented yet */) + ".")
        addSymbol()
      }

    this
  }

  /** Append the terminal symbol to the string that is currently being added.
    * 
    * After calling this, no further appends can be made to the string.
    * @return This object.
    * @throws IllegalStateException if the current string has already been terminated
    */
  def terminate(): this.type = {
    if (isTerminated)
      throw new IllegalStateException("The current string has already been terminated.")

    element = alphabet.sentinel
    isTerminated = true
    if (!storedString.isEmpty) {
      // Start the extensions from the beginning, since we're only now adding this symbol to the string.
      startingExtension = 1
  
      // Reset the last end.
      lastEnd = leaf1.parent
      lastEndOffset = leaf1.length(phase)
  
      addSymbol()
    }
    else { // nothing to add to the tree
      storedString += element
      phase += 1
    }

    this
  }

  /** Append the current `element` to the tree. */
  private[this] def addSymbol() {
    storedString += element
  
    phase += 1
    if (phase == 0) { // build the first implicit tree
      leaf1 = Leaf[I](_root, 0, 0)
      _root.put(element, leaf1)
      startingExtension = 1
    }
    else
      doPhase()

    lastEndOffset += 1 // Move pointer from the end of S[j_{i+1}..i] to S[j_{i+1}..i+1], both on a leaf.
  }

  /** Append the given string to the tree's current string. */
  def +=(string: IndexedSeq[alphabet.External]): this.type = append(string)

  /** The size of the tree, as the number of characters in the string it represents. */
  def size: Int = storedString.length -
    (if (isTerminated) 1 else 0)  // Don't count the termination symbol.

  /** The root node of the tree. */
  def root: RootNode[I] = _root

  /** Return info for debugging. */
  override def toString: String =
    classOf[SuffixTree[_]].getSimpleName + "{\"" +
      {val b = new StringBuilder; storedString.take(10).addString(b); b} +
      (if (storedString.length > 10) "..." else "") +
      "\"}"

  /** Perform the current phase.
    * 
    * Uses the current element of the string.
    * Updates the node and offset that identifies the first leaf.
    * 
    * Implements the "SPA" of §6.1.5 of Gusfield.
    */
  private[this] def doPhase() {
    assert(phase > 0) // the first implicit tree already exists
    if (phase % 16384 == 0) logger.info("Starting phase " + phase + ".")

    // Initialize SEA.
    newInternalNode = null

    /* SPA step 2:
     * Start the extensions after the last explicit extension of the last phase,
     * loop until the first extension where Rule 3 applies, or the last extension.
     */

    // Don't add the termination symbol alone as a suffix.
    val lastExtension = if (element == alphabet.sentinel) phase - 1 else phase

    var rule3 = false
    breakable {
      for (i <- startingExtension to lastExtension) {
        extension = i
        rule3 = extendViaSuffixLink()
        if (rule3) break()
      }
    }
    
    // SPA step 3: Start the next extension after the last "explicit extension".
    startingExtension = extension + (if (rule3) 0 else 1)
  }

  /** Perform the current extension, using the first suffix link found.
    * Leaves `lastEnd` and `lastEndOffset` set according to the resulting extension.
    * 
    * Implements the "SEA" of §6.1.3 of Gusfield.
    * 
    * @return whether extension rule 3 was applied
    */
  private[this] def extendViaSuffixLink(): Boolean = {
    assert(lastEndOffset >= 0)

    // SEA step 1: Find the first node upwards, which is either the root or has a suffix link.
    var node = lastEnd
    var pathLength = lastEndOffset

    /* An internal node created in the last extension won't have a suffix link yet.
     * See Gusfield, "Following a trail of suffix links to build I_{i+1}", at end.
     */
    if (!node.hasSuffixLink && node != _root) {
      pathLength += node.length
      node = node.parent
    }

    // SEA step 2: If not at the root, traverse the suffix link.
    if (node == _root) pathLength -= 1  // Remove S[j-1], to search for S[j..i].
    else node = node.suffixLink

    val (rule3Node, createdInternalNode) = extend(node, pathLength)
    val rule3Applied = rule3Node != null
    assert(lastEndOffset >= 0)
    assert(!(rule3Node != null && createdInternalNode != null))
 
    // SEA step 4: Create a suffix link if an internal node was created in the last extension.
    if (newInternalNode != null) {
      assert(lastEndOffset == 0)  // α must end at an internal node, by Gusfield's Lemma 6.1.1.

      if (rule3Applied) {  // `lastEnd` wasn't changed, since we need to rerun this extension number in the next phase.
        /* Since the last extension created a new internal node, it also created a one-character leaf from it.
         * And since rule 3 was applied in this extension, the node from which it extended already existed, per the proof of Lemma 6.1.1.
         * This node is the target for the suffix link.
         */
        newInternalNode.suffixLink = rule3Node
      }
      else
        newInternalNode.suffixLink = lastEnd
    }

    newInternalNode = createdInternalNode
    rule3Applied
  }

  /** Perform the current extension (by the current element) from the given node.
    * Leaves `lastEnd` and `lastEndOffset` set according to the resulting extension;
    * these aren't updated if extension rule 3 is used,
    * since that extension number will be repeated in the next phase.
    * 
    * Implements steps 2 to 3 of the "SEA" of §6.1.3 of Gusfield.
    * 
    * @param node
    *   The node from which to continue the current extension.
    * @param pathLength
    *   The length of the edge-label γ, the path from the given node to the end of the current
    *   extension's suffix (''S''[''j''..''i''], for phase ''i''+1 and extension ''j'').
    * @return a triple of:
    *   $ 1. if extension rule 3 applied, the internal node above it; else `null`
    *   $ 1. any internal node that was created, else `null`
    */
  @tailrec
  private[this]
  def extend(node: InternalNode[I], pathLength: Int): (InternalNode[I], InternalNode[I]) = {
    assert(node != null)
    assert(pathLength >= 0, "pathLength == " + pathLength)

    if (pathLength == 0) {  // Reached the end of S[j..i].
      if (!node.hasChild(element)) {  // suffix extension rule 2a
        lastEnd = node
        lastEndOffset = 0

        node.addLeaf(element, phase, extension)
        (null, null)
      }
      else  // suffix extension rule 3
        (node, null)
    }
    else {  // Not at the end of S[j..i] yet; walk down the edge.
      // The path always exists already, on this edge:
      val nextElementOnEdge = storedString(phase - pathLength)
      node.child(nextElementOnEdge) match {
        case child: Leaf[I] => {
          val childLength = child.length(phase) - 1  // current, not end-of-phase, length
          assert(childLength >= pathLength)
          if (childLength == pathLength) {  // S[j..i] ends at a leaf: suffix extension Rule 1
            lastEnd = node
            lastEndOffset = pathLength
            (null, null)
          }
          else  // Otherwise, S[j..i] ends inside this edge.
            extendFromInsideAnEdge(node, pathLength, child)
        }
        case child: InternalNode[I] => {
          val excessLength = pathLength - child.length
          if (excessLength >= 0)  // S[j..i] ends on or after the next internal node.
            return extend(child, excessLength)  // use "Trick 1", the "skip/count trick"
          else  // Otherwise, S[j..i] ends inside this edge.
            extendFromInsideAnEdge(node, pathLength, child)
        }
      }
    }
  }

  /** Perform the current extension (by the current element) from the given node, from inside an edge.
    * Leaves `lastEnd` and `lastEndOffset` set according to the resulting extension;
    * these aren't updated if extension rule 3 is used,
    * since that extension number will be repeated in the next phase.
    * 
    * @param node
    *   The node from which to continue the current extension.
    * @param pathLength
    *   The length of the edge-label γ, the path from the given node to the end of the current
    *   extension's suffix (''S''[''j''..''i''], for phase ''i''+1 and extension ''j'').
    * @param child The child node on whose edge the extension happens.
    * @return a pair of: (1) whether extension rule 3 was applied, (2) any internal node that was created
    */
  private[this]
  def extendFromInsideAnEdge(node: InternalNode[I], pathLength: Int, child: Node[I]): (InternalNode[I], InternalNode[I]) = {
    assert(pathLength > 0)
    val nextEdgeChar = storedString(child.edgeStart + pathLength)

    if (element != nextEdgeChar) {  // suffix extension Rule 2b
      val firstEdgeChar = storedString(child.edgeStart)
      val newNode = child.split(firstEdgeChar, nextEdgeChar, pathLength, children)
      newNode.addLeaf(element, phase, extension)

      lastEnd = newNode
      lastEndOffset = 0
      (null, newNode)
    }
    else  // suffix extension Rule 3
      (node, null)
  }

  /**
   * Return a Graphviz representation of this tree.
   * 
   * This is only intended for debugging, as it has no synchronization to guarantee a consistent view of the tree.
   */
  def dot(out: java.io.PrintStream) {
    out.print("strict digraph GST {\n" +
      "node [label=\"\",height=0,width=0];\n" +
      "\"\" [label=root,style=bold];\n")
    dotNode(out, _root, "")
    out.print("}\n")
  }

  /**
   * Add the Graphviz representation of the given node to the given string.
   * 
   * @param nodeLabel
   *      The label of the node.
   * @see #dot()
   */
  private[this]
  def dotNode(out: java.io.PrintStream, node: InternalNode[I], nodeLabel: String) {
    if (node.hasSuffixLink)
      out.print( "\"" + nodeLabel + "\" -> \"" + nodeLabel.substring(1) + "\" [style=dashed,arrowhead=empty,color=red];\n" )

    if (node == newInternalNode)
      out.print( "\"newInternalNode\" [label=\"new internal node\",fontcolor=green,shape=plaintext];\n" +
        "\"newInternalNode\" -> \"" + nodeLabel + "\" [style=bold,color=green];\n" )

    if (node == lastEnd)
      out.print("\"lastEndPosition\" [label=\"last end\\nphase=" + phase +
        "\\nextension=" + extension +
        "\\nlastEndOffset=" + lastEndOffset +
        "\",fontcolor=blue,shape=plaintext];\n" +
        "\"lastEndPosition\" -> \"" + nodeLabel + "\" [style=bold,color=blue];\n")

    for (child <- node.values) {
      val size = storedString.length
      val length = child match {
        case n: Leaf[I] => n.length(phase)
        case n: InternalNode[I] => n.length
      }

      var last = nodeLabel
      val current = new StringBuilder(nodeLabel)
      for (i <- 0 until length) {
        val pos = child.edgeStart + i
        val edge = storedString(pos).toString
        current.append(edge)

        val c = current.toString
        if (i == length - 1)
          out.print( "\"" + c + "\" [label=\"" + c + "\"];\n" )

        out.print( "\"" + last + "\" -> \"" + c + "\" [label=\"" + edge + "\"" +
          (if (i == length - 1) "" else ",arrowhead=none") +
          "];\n" )

        last = c
      }

      if (child.isInstanceOf[InternalNode[I]])
        dotNode(out, child.asInstanceOf[InternalNode[I]], last)
    }
  }
}

/** Factory for suffix tree creation */
object SuffixTree {
  /** Create a suffix tree.
    * 
    * @tparam A The type of the string's alphabet.
    * @param alphabet Alphabet of the string symbols.
    * @example
    * {{{
    * val alphabet = new NucleotideAlphabet()
    * val tree = SuffixTree(alphabet) += "ACCTGACGG"
    * tree.terminate()
    * }}}
    */
  def apply[A <: Alphabet](alphabet: A)(implicit tag: ClassTag[A#Internal]): SuffixTree[A] =
    new SuffixTree[A](alphabet)
}
