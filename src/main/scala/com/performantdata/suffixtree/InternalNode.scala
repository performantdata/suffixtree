/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree;

/** Internal node of a suffix tree.
  *
  * @constructor Create an internal node having the given parent and edge label indices.
  * @tparam I Type of the internal representation of a symbol.
  * @param _parent Parent node. Set to `null` for the root node.
  * @param children Child nodes, keyed by the internal node and the first element of their label.
  * @param _edgeStart Starting position, in the string, of the edge label. Set to `0` for the root node.
  * @param _edgeEnd One after the ending position, in the string, of the edge label. Set to `0` for the root node.
  */
class InternalNode[I] protected[suffixtree] (
  _parent: InternalNode[I],
  children: TwoKeyOpenHashMap[InternalNode[I], I, Node[I]],
  _edgeStart: Int,
  _edgeEnd: Int
  ) extends Node[I](_parent, _edgeStart)
{
  assert(_edgeEnd >= _edgeStart, "Edge length must be non-negative.")

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
    assert(!children.contains(this, element))
    children(this, element) = newLeaf
    newLeaf
  }

  /** The child node whose edge label starts with the given element.
    * @return `null` if there is no such child
    */
  private[suffixtree]
  def child(element: I): Node[I] = children(this, element)

  /** Tests whether there is a the child node whose edge label starts with the given element. */
  private[suffixtree]
  def hasChild(element: I): Boolean = children.contains(this, element)

  /** Add a child node.
    * 
    * @param kv A pair: (1) the first character of the label of the edge leading to the child node; (2) the child node.
    */
  private[suffixtree]
  def put(kv: (I, Node[I])): Option[Node[I]] = {
    assert(kv._2 != this)
    val oldNode = children.get(this, kv._1)
    children(this, kv._1) = kv._2
    oldNode
  }

  /** The child nodes of this node. */
  def values: Iterable[Node[I]] = children.get(this).values

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
    * @param children Child nodes, keyed by the internal node and the first element of their label.
    * @param edgeStart Starting position, in the string, of the edge label.
    * @param edgeLength Number of characters on the edge from the parent node to this node.
    */
  def apply[I](
    parent: InternalNode[I],
    children: TwoKeyOpenHashMap[InternalNode[I], I, Node[I]],
    edgeStart: Int,
    edgeLength: Int
  ): InternalNode[I] = {
    assert(parent != null, "Parent reference cannot be empty.")
    assert(edgeStart >= 0, "Edge start must be non-negative.")
    assert(edgeLength > 0, "Length must be positive.")

    new InternalNode(parent, children, edgeStart, edgeStart + edgeLength)
  }
}
