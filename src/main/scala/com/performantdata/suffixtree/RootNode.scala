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
  */
final class RootNode[I] private extends InternalNode[I](null, 0, 0) {
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
    */
  def apply[I]() = new RootNode[I]
}
