/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package com.performantdata.collection.mutable

import scala.collection.mutable.AbstractMap
import scala.collection.mutable.Map
import scala.collection.mutable.MapLike
import scala.collection.mutable.HashEntry
import scala.collection.AbstractIterator

/**
 *  @define Coll `OpenHashMap`
 *  @define coll open hash map
 *
 *  @since 2.7
 */
object OpenHashMap {
  /** The states of a slot in the hash table */
  private val Empty: Byte = 0
  private val Deleted: Byte = 1
  private val Occupied: Byte = 2

  def apply[K, V](elems : (K, V)*) = new OpenHashMap[K, V] ++= elems
  def empty[K, V] = new OpenHashMap[K, V]

  final private class OpenEntry[Key, Value](val key: Key,
                                            val hash: Int,
                                            var value: Value)
                extends HashEntry[Key, OpenEntry[Key, Value]]

  private[mutable] def nextPositivePowerOfTwo(i : Int) = 1 << (32 - Integer.numberOfLeadingZeros(i - 1))
}

/** A mutable hash map based on an open hashing scheme. The precise scheme is
 *  undefined, but it should make a reasonable effort to ensure that an insert
 *  with consecutive hash codes is not unnecessarily penalised. In particular,
 *  mappings of consecutive integer keys should work without significant
 *  performance loss.
 *
 *  @tparam Key          type of the keys in this map.
 *  @tparam Value        type of the values in this map.
 *  @param initialSize   the initial size of the internal hash table.
 *
 *  @author David MacIver
  * @author Michael, Performant Data LLC
 *  @since  2.7
 *
 *  @define Coll `OpenHashMap`
 *  @define coll open hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
class OpenHashMap[Key, Value](initialSize : Int)
extends AbstractMap[Key, Value]
   with Map[Key, Value]
   with MapLike[Key, Value, OpenHashMap[Key, Value]] {

  import OpenHashMap._
  private type Entry = OpenEntry[Key, Value]

  /** A default constructor creates a hashmap with initial size `8`.
   */
  def this() = this(8)

  override def empty: OpenHashMap[Key, Value] = OpenHashMap.empty[Key, Value]

  private[this] val actualInitialSize = OpenHashMap.nextPositivePowerOfTwo(initialSize)

  private var mask = actualInitialSize - 1
  private var table = new Array[Entry](actualInitialSize)
  /** States of the corresponding key/value table entries */
  private var states = new Array[Byte](actualInitialSize)
  private var _size = 0
  private var deleted = 0

  // Used for tracking inserts so that iterators can determine in concurrent modification has occurred.
  private[this] var modCount = 0

  override def size = _size
  private[this] def size_=(s : Int) { _size = s }

  /** Returns a mangled hash code of the provided key. */
  protected def hashOf(key: Key) = {
    var h = key.##
    h ^= ((h >>> 20) ^ (h >>> 12))
    h ^ (h >>> 7) ^ (h >>> 4)
  }

  /** Increase the size of the tables.
    * Copy over the occupied slots, effectively eliminating the deleted slots.
    */
  private[this] def growTable() = {
    val oldSize = mask + 1
    val newSize = 4 * oldSize

    val oldTable = table
    val oldStates = states
    table = new Array[Entry](newSize)
    states = new Array[Byte](newSize)

    mask = newSize - 1
    var i = oldStates.length
    while ({ i -= 1; i >= 0 })
      if (oldStates(i) == Occupied) {
        val entry = oldTable(i)
        val index = findIndex(entry.key, entry.hash)
        table(index) = entry
        states(index) = Occupied
      }

    deleted = 0
  }

  /** Return the index of the first slot in the hash table (in probe order)
    * that either is empty, or is or was last occupied by the given key.
    */
  private[this] def findIndex(key: Key) : Int = findIndex(key, hashOf(key))

  /** Return the index of the first slot in the hash table (in probe order)
    * that either is empty, or is or was last occupied by the given key.
    * 
    * This method is an optimization for when the hash value is in hand.
    * 
    * @param hash hash value for `key`
    */
  private[this] def findIndex(key: Key, hash: Int): Int = {
    var j = hash

    var index = hash & mask
    var perturb = index
    while(table(index) != null &&
          !(table(index).hash == hash &&
            table(index).key == key)){
      j = 5 * j + 1 + perturb
      perturb >>= 5
      index = j & mask
    }
    index
  }

  override def update(key: Key, value: Value) {
    put(key, hashOf(key), value)
  }

//  @deprecatedOverriding("+= should not be overridden in order to maintain consistency with put.", "2.11.0")
  def += (kv: (Key, Value)): this.type = { put(kv._1, kv._2); this }
  
//  @deprecatedOverriding("-= should not be overridden in order to maintain consistency with remove.", "2.11.0")
  def -= (key: Key): this.type = { remove(key); this }

  override def put(key: Key, value: Value): Option[Value] =
    put(key, hashOf(key), value)

  private def put(key: Key, hash: Int, value: Value): Option[Value] = {
    if (2 * (size + deleted) > mask) growTable()
    val index = findIndex(key, hash)
    states(index) match {
      case Empty =>
        table(index) = new OpenEntry(key, hash, value)
        states(index) = Occupied
        modCount += 1
        size += 1
        None

      case Deleted =>
        table(index).value = value
        states(index) = Occupied
        size += 1
        modCount += 1
        //FIXME deleted -= 1
        None

      case Occupied =>
        val entry = table(index)
        val res = entry.value
        entry.value = value
        Some(res)
    }
  }

  override def remove(key : Key): Option[Value] = {
    val index = findIndex(key)
    if (states(index) == Occupied) {
      states(index) = Deleted
      size -= 1
      deleted += 1
      Some(table(index).value)
    } else None
  }

  def get(key : Key) : Option[Value] = {
    val hash = hashOf(key)

    var j = hash
    var index = hash & mask
    var perturb = index
    var entry = table(index)
    while(entry != null){
      if (entry.hash == hash &&
          entry.key == key){
        return if (states(index) == Deleted) None else Some(entry.value)
      }

      j = 5 * j + 1 + perturb
      perturb >>= 5
      index = j & mask
      entry = table(index)
    }
    None
  }

  /** An iterator over the elements of this map. Use of this iterator follows
   *  the same contract for concurrent modification as the foreach method.
   *
   *  @return   the iterator
   */
  def iterator: Iterator[(Key, Value)] = new AbstractIterator[(Key, Value)] {
    var index = 0
    val initialModCount = modCount

    private[this] def advance() {
      if (initialModCount != modCount) sys.error("Concurrent modification")
      while (index <= mask && states(index) != Occupied) index += 1
    }

    def hasNext: Boolean = {advance(); index <= mask }

    def next: (Key, Value) = {
      advance()
      //FIXME Throw exception if index > mask.
      val result = table(index)
      index += 1
      (result.key, result.value)
    }
  }

  override def clone() = {
    val it = new OpenHashMap[Key, Value]
    foreachUndeletedEntry(entry => it.put(entry.key, entry.hash, entry.value))
    it
  }

  /** Loop over the key, value mappings of this map.
   *
   *  The behaviour of modifying the map during an iteration is as follows:
   *  - Deleting a mapping is always permitted.
   *  - Changing the value of mapping which is already present is permitted.
   *  - Anything else is not permitted. It will usually, but not always, throw an exception.
   *
   *  @tparam U  The return type of the specified function `f`, return result of which is ignored.
   *  @param f   The function to apply to each key, value mapping.
   */
  override def foreach[U](f : ((Key, Value)) => U) {
    val startModCount = modCount
    foreachUndeletedEntry(entry => {
      if (modCount != startModCount) sys.error("Concurrent Modification")
      f((entry.key, entry.value))}
    )
  }

  private[this] def foreachUndeletedEntry(f : Entry => Unit){
    var i = states.length
    while ({ i -= 1; i >= 0 })
      if (states(i) == Occupied)
        f(table(i))
  }

  override def transform(f : (Key, Value) => Value) = {
    foreachUndeletedEntry(entry => entry.value = f(entry.key, entry.value))
    this
  }

  override def retain(f : (Key, Value) => Boolean) = {
    var i = states.length
    while ({ i -= 1; i >= 0 })
      if (states(i) == Occupied) {
        val entry = table(i)
        if (!f(entry.key, entry.value)) {
          states(i) == Deleted
          size -= 1
          deleted += 1
          //FIXME modCount += 1
        }
      }

    this
  }

  override def stringPrefix = "OpenHashMap"
}
