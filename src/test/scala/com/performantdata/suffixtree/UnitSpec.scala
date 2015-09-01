/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.annotation.elidable

/** Base class for unit tests in this project.
  * 
  * @author Michael
  */
abstract class UnitSpec extends FlatSpec with Matchers {
  /** Tests whether assertions are enabled. */
  @elidable(elidable.ASSERTION) protected def assertionsEnabled = true
}
