/*
 * Copyright © 2015 Performant Data LLC. All rights reserved.
 */
package com.performantdata.suffixtree

import scala.collection.{AbstractMap, Map, MapLike}
import scala.collection.AbstractIterator
import scala.reflect.ClassTag
import java.util.ConcurrentModificationException
import scala.collection.mutable.AnyRefMap
import scala.collection.mutable.OpenHashMap
import scala.collection.DefaultMap

object TwoKeyOpenHashMap {
  /** The state of a slot in the hash table */
  private object SlotState extends Enumeration {
    type SlotState = Value
    val Empty, Deleted, Occupied = Value
  }

  def empty[K1 <: AnyRef : ClassTag, K2: ClassTag, V: ClassTag] = new TwoKeyOpenHashMap[K1, K2, V]

  /** Minimum number of bits of addressing space in a bucket */
  private val minimumBucketSpaceBits = 1

  /** Maximum number of bits of addressing space in a hash table */
  private val maximumTableSpaceBits = Integer.SIZE - 2

  /** Minimum number of bits of addressing space in a hash table */
  private val minimumTableSpaceBits = 3
}

/** A mutable open-address hash map that performs hashing at two levels.
  * 
  * Superficially, this is a map whose values are maps.
  * There is a performance advantage, though, to accessing values by both keys at once,
  * in that the intermediate map needn't be instantiated.
  * 
  * The map has a capacity, initially influenced by the `requestedCapacity`,
  * that is the number of values it can hold.
  * The map may attempt to resize before this limit is reached, if it is having difficulty finding available space.
  * 
  * The map has a separate capacity for the space of the second key.
  * For good memory locality,
  * this should be made large enough to store most of the sets of values found within a first-level key.
  * 
  * @tparam K1 Type of the first-level keys in this map.
  * @tparam K2 Type of the second-level keys in this map.
  * @tparam V Type of the values in this map.
  * @param requestedCapacity The requested initial capacity.
  * @param secondKeyRequestedCapacity
  *   The requested capacity for the space of the second key.
  *   Must be at least a factor of two less than `requestedCapacity`.
  * 
  * @author Michael, Performant Data LLC
  */
class TwoKeyOpenHashMap[K1 <: AnyRef : ClassTag, K2: ClassTag, V: ClassTag](
  requestedCapacity: Int = 1 << TwoKeyOpenHashMap.minimumTableSpaceBits,
  secondKeyRequestedCapacity: Int = 1 << TwoKeyOpenHashMap.minimumBucketSpaceBits
) {
  import TwoKeyOpenHashMap._
  import SlotState._

  /** Number of bits of addressing space in a bucket */
  private[this] val bucketBits =
    (Integer.SIZE - Integer.numberOfLeadingZeros(secondKeyRequestedCapacity - 1))
      .max(minimumBucketSpaceBits)
      .min(maximumTableSpaceBits - 1)  // make room for at least one bit of addressing in the first key space

  /** Number of bits of addressing space in the hash table */
  private var tableBits =
    (Integer.SIZE - Integer.numberOfLeadingZeros(requestedCapacity - 1))
      .max(minimumTableSpaceBits)
      .max(bucketBits + 1)  // make room for at least one bit of addressing in the first key space
      .min(maximumTableSpaceBits)

  /** Maximum number of values that can be stored in a bucket */
  private[this] val bucketCapacity = 1 << bucketBits

  /** Maximum number of values that can be stored in the hash table when the map was created */
  private[this] val initialTableCapacity = 1 << tableBits

  /** Hash code mask for second key, to convert it to an offset within a bucket. */
  private[this] val bucketMask = bucketCapacity - 1

  /** Hash code mask for first key, to convert it to a bucket location with the table. */
  private var tableMask = (initialTableCapacity - 1) ^ bucketMask

  /** Lowest bit of `tableMask` */
  private[this] val tableMaskLowBit = bucketCapacity

  /** Number of buckets */
  private var bucketCount = initialTableCapacity / bucketCapacity

  /** First-key table */
  private var key1s = new Array[K1](initialTableCapacity)
  /** Second-key table */
  private var key2s = new Array[K2](initialTableCapacity)
  /** Value table */
  private var vals = new Array[V](initialTableCapacity)
  /** State of the corresponding key/value table entries */
  private var states = new Array[Byte](initialTableCapacity)

  private[this] object state {
    def apply(i: Int) = SlotState(states(i))
    def update(i: Int, s: SlotState) { states(i) = s.id.toByte }
  }

  /** Number of values stored, by first key */
  private var key1Sizes = AnyRefMap.empty[K1, Int]

  /** Number of values stored */
  private var _size = 0
  def size = _size

  private[this] var deletedCount = 0

  /** Used for tracking inserts for concurrent modification.
    * The actual value doesn't matter, just whether it changes.
    */
  private[this] var modifications = 0

  /** Compute the first hash in the table-level double-hashing scheme.
    * 
    * @param key1Hash the hash code of the first key
    */
  private[this] def level1Hash1(key1Hash: Int) = (key1Hash * 0x3595e74b >>> Integer.SIZE - tableBits) & tableMask

  /** Compute the second hash in the table-level double-hashing scheme.
    * 
    * @param key1Hash the hash code of the first key
    */
  private[this] def level1Hash2(key1Hash: Int) = (
    (key1Hash * 0x208c7cb3 >>> Integer.SIZE - bucketBits) | tableMaskLowBit  // make it odd so it's relatively prime to (any power of) 2
    ) & tableMask

  /** Scan the hash table using the given key pair, performing an action.
    * 
    * The keys determine which slots in the table are examined and in what order.
    * The action is performed on each examined slot,
    * until the action says to stop, or until all slots for these keys have been examined.
    * 
    * @param f the action to perform; accepts a table index, returns whether scanning should stop.
    */
  private[this] def probe(k1: K1, k2: K2, f: (Int) => Boolean) {
    val key1Hash = k1.##
    val key2Hash = k2.##

    /* Hash the native hash codes a bit to address their shortcomings;
     * they don't always produce well-distributed values, especially for primitives.
     * 
     * The 1st-level hash must depend only on the first key, for locality,
     * but the 2nd-level key can benefit from the admixture.
     * 
     * The multipliers aren't too special, just random odd integers.
     * The right-shifts capture the higher bits, which have better mixing.
     */

    // double hashing to find bucket: h(k1,i) = (h1(k1) + i*h2(k1)) mod 2^bucketCount
    var bucketStart = level1Hash1(key1Hash)
    lazy val level1hash2 = level1Hash2(key1Hash)

    val level2Hash = (key1Hash + key2Hash) * 0x561cd9b5

    for (i ← 0 until bucketCount) {
      var offset = level2Hash 
      for (j ← 0 until bucketCapacity) {
        if (f(bucketStart + (offset & bucketMask))) return
        offset += 1  // linear probing to find slot in bucket
      }

      bucketStart = (bucketStart + level1hash2) & tableMask  // Double-hash to the next bucket.
    }
  }

  /** Optionally return the value associated with the keys.
    * @return if a value is associated to these keys, an option containing the value, else `None`
    */
  def get(k1: K1, k2: K2): Option[V] = {
    var ret: Option[V] = None

    def f(index: Int): Boolean = state(index) match {
      case Empty => true
      case Occupied =>
        if (k1 == key1s(index) && k2 == key2s(index)) { ret = Some(vals(index)); true }
        else false
      case _ => false
    }

    probe(k1, k2, f)
    ret
  }

  /** Retrieves that value associated with the given keys.
    * Invokes the `default` method if there is no mapping from the given keys.
    */
  def apply(k1: K1, k2: K2): V = get(k1, k2).getOrElse(default(k1, k2))

  /** Defines the default value computation for the map, returned when a key is not found.
    * One might override this method in subclasses.
    * @throws NoSuchElementException always
    */
  def default(k1: K1, k2: K2): V = throw new NoSuchElementException

  /** Tests whether this map contains a value for the keys. */
  def contains(k1: K1, k2: K2): Boolean = {
    var ret = false

    def f(index: Int): Boolean = state(index) match {
      case Empty => true
      case Occupied =>
        if (k1 == key1s(index) && k2 == key2s(index)) { ret = true; true }
        else false
      case _ => false
    }

    probe(k1, k2, f)
    ret
  }

  private def put(k1: K1, k2: K2, value: V): Option[V] = {
    if (2 * (_size + deletedCount) > vals.length)  //TODO Refine load factor.
      grow()

    var ret: Option[V] = null
    var firstDeletedIndex: Option[Int] = None

    def f(index: Int): Boolean = state(index) match {
      case Occupied =>
        if (k1 == key1s(index) && k2 == key2s(index)) {
          ret = Some(vals(index))
          vals(index) = value
          // This isn't considered a modification, for the purpose of iterators?
          true
        }
        else false
      case Deleted => { firstDeletedIndex = Some(index); false }
      case Empty => {
        if (firstDeletedIndex.isDefined) deletedCount -= 1
        val i = firstDeletedIndex.getOrElse(index)
        key1s(i) = k1
        key2s(i) = k2
        vals(i) = value
        state(i) = Occupied
        _size += 1
        key1Sizes(k1) = key1Sizes.getOrElse(k1, 0) + 1
        modifications += 1
        ret = None
        true
      }
    }

    probe(k1, k2, f)
    if (ret == null)  throw new IllegalStateException("Unable to store a value. No space available?")
    ret
  }

  def update(k1: K1, k2: K2, value: V): Option[V] = put(k1, k2, value)

  private[this] def remove(k1: K1, k2: K2): Option[V] = {
    var ret: Option[V] = None

    def f(index: Int): Boolean = state(index) match {
      case Occupied =>
        if (k1 == key1s(index) && k2 == key2s(index)) {
          state(index) = Deleted
          _size -= 1
          key1Sizes(k1) = key1Sizes(k1) - 1
          modifications += 1
          deletedCount += 1
          ret = Some(vals(index))
          true
        }
        else false
      case _ => false
    }

    probe(k1, k2, f)
    ret
  }

  /** View of the second-level key mapping relative to a particular first-level key.
    * 
    * @param k1 first-level key on which this map is based
    */
  private[this] class MapView(k1: K1) extends DefaultMap[K2, V] {
    def get(key: K2): Option[V] = TwoKeyOpenHashMap.this.get(k1, key)

    def iterator(): Iterator[(K2, V)] = new AbstractIterator[(K2, V)] {
      private[this] val initialModifications = modifications
      private[this] var remaining = key1Sizes(k1)
      private[this] val key1Hash = k1.##
      private[this] var bucketStart = level1Hash1(key1Hash)
      private[this] val level1hash2 = level1Hash2(key1Hash)
      private[this] var bucket = 0
      private[this] var index = bucketStart

      /** Skip the table slots that don't contain a value for `k1`, advancing `index` to the next one that does. */
      private[this] def skipNotOurs() {
        if (initialModifications != modifications)
          throw new ConcurrentModificationException

        while (bucket < bucketCount) {
          while (index < bucketStart + bucketCapacity) {
            if (state(index) == Occupied && k1 == key1s(index)) return
            index += 1
          }

          bucket += 1
          bucketStart = (bucketStart + level1hash2) & tableMask  // Double-hash to the next bucket.
          index = bucketStart
        }

        throw new IllegalStateException("Remaining elements expected but not found.")
      }

      def hasNext(): Boolean =
        if (remaining == 0) false
        else {
          skipNotOurs()
          true
        }

      def next(): (K2, V) =
        if (remaining == 0) throw new NoSuchElementException
        else {
          skipNotOurs()
          val r = (key2s(index), vals(index))
          index += 1
          remaining -= 1
          r
        }
    }
  }

  def get(key: K1): Map[K2, V] = new MapView(key)

  def empty: TwoKeyOpenHashMap[K1, K2, V] = TwoKeyOpenHashMap.empty[K1, K2, V]

  /** Increase the capacity. */
  private[this] def grow() = {
    val oldCapacity = vals.length
    val newCapacity = oldCapacity << (if (oldCapacity <= 65536) 2 else 1)  // grow faster if small
    if (newCapacity > (1 << maximumTableSpaceBits) || newCapacity < 0)
      throw new ArrayIndexOutOfBoundsException("Requested size too large") //FIXME Need alternative for overflow.

    // copy the entries to a new map
    val map = new TwoKeyOpenHashMap[K1, K2, V](newCapacity, bucketCapacity)
    var i = 0
    while (i < oldCapacity) {
      if (state(i) == Occupied) map.put(key1s(i), key2s(i), vals(i))
      i += 1
    }

    // move the new map's data into this one
    key1s       = map.key1s
    key2s       = map.key2s
    vals        = map.vals
    states      = map.states
    key1Sizes   = map.key1Sizes
    _size       = map._size
    deletedCount = 0
    tableMask   = map.tableMask
    tableBits   = map.tableBits
    bucketCount = map.bucketCount
  }
}
