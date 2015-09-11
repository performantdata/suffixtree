/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata

/** Suffix tree implementation.
  * 
  * The main class is [[SuffixTree]].
  */
package object suffixtree {
  private[suffixtree] implicit val nodeClassTag = scala.reflect.classTag[Node[_]]
}
