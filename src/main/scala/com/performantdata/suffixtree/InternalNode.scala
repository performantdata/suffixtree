/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree;

import scala.collection._
import scala.collection.mutable.HashTable
import scala.collection.mutable.DefaultEntry


/** Internal node of a suffix tree.
  *
  * @constructor Create an internal node having the given parent and edge label indices.
  * @tparam I Type of the internal representation of a symbol.
  * @param _parent Parent node. Set to `null` for the root node.
  * @param _edgeStart Starting position, in the string, of the edge label. Set to `0` for the root node.
  * @param _edgeEnd One after the ending position, in the string, of the edge label. Set to `0` for the root node.
  */
class InternalNode[I] protected[suffixtree] (_parent : InternalNode[I], _edgeStart : Int, _edgeEnd : Int)
  extends Node[I](_parent, _edgeStart)
  with HashTable[I, DefaultEntry[I, Node[I]]]  // Holds the child nodes of this node, keyed by the first element of their label.
{
  assert(_edgeEnd >= _edgeStart, "Edge length must be non-negative.")

  protected def createNewEntry[B1](key: I, value: B1): DefaultEntry[I, Node[I]] =
    new DefaultEntry(key, value.asInstanceOf[Node[I]])

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
    val e = findOrAddEntry(element, newLeaf)
    assert(e == null)
    newLeaf
  }

  /** The child node whose edge label starts with the given element.
    * @return `null` if there is no such child
    */
  private[suffixtree]
  def child(element: I): Node[I] = {
    val result = findEntry(element)
    if (result eq null) null else result.value
  }

  /** Tests whether there is a the child node whose edge label starts with the given element. */
  private[suffixtree]
  def hasChild(element: I): Boolean = findEntry(element) != null

  /** Add a child node.
    * 
    * @param kv A pair: (1) the first character of the label of the edge leading to the child node; (2) the child node.
    */
  private[suffixtree]
  def put(kv: (I, Node[I])): Option[Node[I]] = {
    assert(kv._2 != this)
    val e = findOrAddEntry(kv._1, kv._2)
    if (e eq null) None
    else { val v = e.value; e.value = kv._2; Some(v) }
  }

  /** An unmodifiable map of this node's children,
    * keyed by the first character of the label on the edge leading to the child.
    * 
    * The underlying map may change if the suffix tree is modified.
    */
  def children: scala.collection.Map[I, Node[I]] = {
    val arr = new Array[(I, Node[I])](tableSize)
    var i = 0
    foreachEntry { e => arr(i) = (e.key, e.value); i += 1 }

    Map(arr: _*)
  }

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
  def apply[I](parent: InternalNode[I], edgeStart: Int, edgeLength: Int): InternalNode[I] = {
    assert(parent != null, "Parent reference cannot be empty.")
    assert(edgeStart >= 0, "Edge start must be non-negative.")
    assert(edgeLength > 0, "Length must be positive.")

    new InternalNode(parent, edgeStart, edgeStart + edgeLength)
  }
}
