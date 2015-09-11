/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree;

import debox.Map
import scala.reflect.ClassTag


/** Internal node of a suffix tree.
  *
  * @constructor Create an internal node having the given parent and edge label indices.
  * @tparam I Type of the internal representation of a symbol.
  * @param _parent Parent node. Set to `null` for the root node.
  * @param _edgeStart Starting position, in the string, of the edge label. Set to `0` for the root node.
  * @param _edgeEnd One after the ending position, in the string, of the edge label. Set to `0` for the root node.
  */
class InternalNode[I] protected[suffixtree] (_parent : InternalNode[I], _edgeStart : Int, _edgeEnd : Int)
  (implicit ev: ClassTag[I])
  extends Node[I](_parent, _edgeStart)
{
  assert(_edgeEnd >= _edgeStart, "Edge length must be non-negative.")

  /** Child nodes of this node, keyed by the first element of their label. */
  private[this] val childrenByElement: Map[I, Node[_]] = Map.empty[I, Node[_]](ev, nodeClassTag)

  /** Suffix link, if any, from this node. */
  private[this] var _suffixLink: InternalNode[I] = null

  /** Suffix link, if any, from this node. */
  private[suffixtree] def suffixLink = _suffixLink

  /** Set the suffix link from this node. */
  private[suffixtree] def suffixLink_=(n: InternalNode[I]) {
    assert(n != null && n != this, n)
    _suffixLink = n
  }

  /** The length of the label of the edge leading from the parent to this node. */
  private[suffixtree] def length = _edgeEnd - edgeStart

  /** Add a child leaf.
    * 
    * @param element The (one-element) label of the edge to the new leaf.
    * @param startIndex Start index of the label, in the tree's string.
    * @param number The leaf node number.
    * @return the new leaf
    */
  private[suffixtree]
  def addLeaf(element: I, startIndex: Int, number: Int): Leaf[I] = {
    val newLeaf = Leaf[I](this, startIndex, number)
    assert(!childrenByElement.contains(element))
    childrenByElement(element) = newLeaf
    newLeaf
  }

  /** The child node whose edge label starts with the given element.
    * @return `null` if there is no such child
    */
  private[suffixtree]
  def child(element: I): Node[I] = childrenByElement(element).asInstanceOf[Node[I]]

  /** Tests whether there is a the child node whose edge label starts with the given element. */
  private[suffixtree]
  def hasChild(element: I): Boolean = childrenByElement.contains(element)

  /** Add a child node.
    * 
    * @param kv A pair: (1) the first character of the label of the edge leading to the child node; (2) the child node.
    */
  private[suffixtree]
  def put(kv: (I, Node[I])): Option[Node[I]] = {
    assert(kv._2 != this)
    val oldNode = childrenByElement.get(kv._1)
    childrenByElement(kv._1) = kv._2
    oldNode.asInstanceOf[Option[Node[I]]]
  }

  /** Applies a function to all elements of this node's map.
    * 
    * @param f A function taking a pair of:
    *   (1) the first character on an edge leading to a child node,
    *   (2) the child node on that edge.
    */
  def foreach(f: ((I, Node[I])) => Unit): Unit =
    childrenByElement.foreach((k,v) => f((k,v.asInstanceOf[Node[I]])))

  /** The child nodes of this node. */
  def values: Iterable[Node[I]] = childrenByElement.valuesArray.asInstanceOf[Array[Node[I]]]

  /** Return whether this node has a suffix link from it. */
  private[suffixtree]
  def hasSuffixLink = suffixLink != null

  /** Return info for debugging. */
  override def toString: String =
    classOf[InternalNode[_]].getSimpleName + "{(" + edgeStart + "-" + _edgeEnd + ")" + ", " + parent + "}"
}

private[suffixtree]
object InternalNode {
  /** Create an internal node.
    * 
    * @param parent The parent of the new node.
    * @param edgeStart Starting position, in the string, of the edge label.
    * @param edgeLength Number of characters on the edge from the parent node to this node.
    */
  def apply[I: ClassTag](parent: InternalNode[I], edgeStart: Int, edgeLength: Int): InternalNode[I] = {
    assert(parent != null, "Parent reference cannot be empty.")
    assert(edgeStart >= 0, "Edge start must be non-negative.")
    assert(edgeLength > 0, "Length must be positive.")

    new InternalNode(parent, edgeStart, edgeStart + edgeLength)
  }
}
