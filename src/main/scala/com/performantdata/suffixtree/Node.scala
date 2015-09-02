/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree;

import java.util.List;

/** Node of a suffix tree.
  *
  * A node also encapsulates the edge leading to it from its parent.
  * It does so with a reference to its parent node and a reference to the substring that labels the edge.
  * Only an index to the start of the substring is stored here;
  * the ending index is stored by subclasses:
  * explicitly in the case of an internal node, or implicitly by the phase number in the case of a leaf node.
  * 
  * @param parent Parent node. Set to `null` for the root node.
  * @param _edgeStart Starting position, in the string, of the edge label. Set to `0` for the root node.
  */
abstract class Node[T] protected[suffixtree] (private[suffixtree] var parent: InternalNode[T], private[this] var _edgeStart: Int) {
  assert(_edgeStart >= 0)

  /** Starting position, in the string, of the edge label.
    * For non-root nodes, this value can increase; specifically, when a new node splits the edge.
    * 
    * This implements §6.1.4 of Gusfield, "Edge-label compression".
    * 
    * @return `0` for the root node.
    */
  def edgeStart: Int = _edgeStart

  /** Split the edge leading to this node at the given edge label offset.
    * 
    * The new node will
    * $ - assume this node's parent as its own,
    * $ - assume the first `edgeLength` elements of this node's edge, and
    * $ - become this node's parent.
    * 
    * @param firstEdgeChar
    *   First character of the label on the edge leading to this node.
    * @param nextEdgeChar
    *   Next character of the label on the edge after the split location.
    * @param edgeLength
    *   The length of the edge leading to the new node.
    *   No check is made to insure that this is less than the length of the existing edge.
    * @return the new node that splits the edge
    */
  private[suffixtree]
  def split(firstEdgeChar: T, nextEdgeChar: T, edgeLength: Int): InternalNode[T] = {
    assert(edgeLength > 0)

    // TODO Should I retain the indexing of the child node string instead?
    val newNode = InternalNode[T](parent, _edgeStart, edgeLength)
    val oldNode = parent.put(firstEdgeChar, newNode) // replaces this node in the parent's children
    assert(oldNode.get == this)

    parent = newNode
    _edgeStart += edgeLength;
    newNode.put(nextEdgeChar, this);

    newNode;
  }
}
