/*
 * Copyright © 2015,2020 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.elidable

/** Base class for unit tests in this project.
  * 
  * @author Michael
  */
abstract class UnitSpec extends AnyFlatSpec with Matchers {
  /** Tests whether assertions are enabled. */
  @elidable(elidable.ASSERTION) protected def assertionsEnabled = true
}
