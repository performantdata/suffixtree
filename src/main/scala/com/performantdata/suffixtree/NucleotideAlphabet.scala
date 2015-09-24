/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

/** An alphabet for well-defined nucleotides. */
class NucleotideAlphabet extends Alphabet {
  type External = Char
  type Internal = Char
  val sentinel = '$'
  def convert(s: Char) = s
  def size = 5
}
