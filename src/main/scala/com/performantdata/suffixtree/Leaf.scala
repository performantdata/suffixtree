/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/** Leaf node of a suffix tree. 
  * 
  * @constructor Create a node with the given parent and edge label indices.
  * @param _edgeStart The start index of the leaf's edge label, in the tree's string.
  * @param stringStart The starting position, in the given string, of the suffix that ends at this leaf.
  */
final class Leaf[T] private (_parent: InternalNode[T], _edgeStart: Int, val stringStart: Int) extends Node[T](_parent, _edgeStart) {
  assert(_parent != null && _edgeStart >= 0 && stringStart >= 0)

  /** Return the length, as of the end of the given phase, of the edge to this node (from its parent node).
    * 
    * This allows implementation of step 1 of "SPA" of §6.1.5 of Gusfield.
    * This implements "Trick 3" of §6.1.5 of Gusfield.
    * 
    * @param phase The current phase. The corresponds to ''e''-1 in Gusfield's notation.
    */
  // I pass the phase as a parameter to avoid storing in the node a reference back to the suffix tree.
  def length(phase: Int) = phase + 1 - edgeStart

  /** Return info for debugging. */
  override def toString: String =
    classOf[Leaf[_]].getSimpleName + "{(" + edgeStart + "-)" + ", " + parent + "}"
}

private[suffixtree]
object Leaf {
  /** Create a node with the given parent and edge label indices.
    * 
    * @param parent The parent node to this leaf node. Must be non-null.
    * @param edgeStart The start index of the leaf's edge label, in the tree's string. Must be non-negative.
    * @param stringStart The starting position, in the given string, of the suffix that ends at this leaf. Must be non-negative.
    */
  def apply[I](parent: InternalNode[I], edgeStart: Int, stringStart: Int) = new Leaf(parent, edgeStart, stringStart)
}
