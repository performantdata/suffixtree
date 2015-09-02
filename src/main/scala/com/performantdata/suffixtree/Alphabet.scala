/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

/** An alphabet used in a suffix tree.
  * 
  * In addition to the symbols of the alphabet ("standard symbols"), which one expects to find in a string,
  * an alphabet designates a terminal symbol that is distinct from all standard symbols.
  * An alphabet also identifies the external and internal representations of its symbols,
  * and methods for performing comparisons of strings in the alphabet.
  * 
  * This class would typically be extended by a singleton object representing a new alphabet.
  * @example
  * {{{
  * object NucleotideAlphabet extends Alphabet[Char, Char] {
  *   val terminalSymbol = '$'
  *   def convert(s: Char) = s
  * }
  * }}}
  */
abstract class Alphabet {
  /** The external representation of a standard symbol (for use outside the suffix tree library). */
  type External

  /** The internal representation of a symbol (for use within the suffix tree library). */
  type Internal

  val terminalSymbol: Internal

  /** Convert an external representation of a standard symbol to an internal one. */
  def convert(s: External): Internal
}
