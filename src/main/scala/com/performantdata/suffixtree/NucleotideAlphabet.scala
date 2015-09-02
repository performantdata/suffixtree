/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

/** An alphabet for well-defined nucleotides. */
class NucleotideAlphabet extends Alphabet {
  type External = Char
  type Internal = Char
  val terminalSymbol = '$'
  def convert(s: Char) = s
}
