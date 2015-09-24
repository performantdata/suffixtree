/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

/** A root node of a suffix tree.
  * 
  * I follow the usual convention that a root node is an internal node, for ease of implementation,
  * although Gusfield considers it not to be.
  * 
  * @tparam I Type of the internal representation of a symbol.
  * @param children
  *   Child nodes, keyed by the internal node and the first element of their label.
  */
final class RootNode[I] private (children: TwoKeyOpenHashMap[InternalNode[I], I, Node[I]])
  extends InternalNode[I](null, children, 0, 0)
{
  /** Return the length of the label of the edge leading from the parent to this node.
    * Always returns 0 for the root node.
    */
  private[suffixtree]
  override def length = 0

  /** Return info for debugging. */
  override def toString: String = classOf[RootNode[_]].getSimpleName + "{}"
}

private[suffixtree]
object RootNode {
  /** Create a root node for a suffix tree.
    * 
    * @tparam I Type of the internal representation of a symbol.
    * @param children
    *   Child nodes, keyed by the internal node and the first element of their label.
    */
  def apply[I](children: TwoKeyOpenHashMap[InternalNode[I], I, Node[I]]) = new RootNode(children)
}
