/**
 * Copyright (C) 2016-2017 Lightbend Inc. <http://www.lightbend.com>
 */
package akka.util
import java.util.Arrays
import akka.annotation.InternalApi
import scala.annotation.tailrec

/**
 * INTERNAL API
 */
@InternalApi private[akka] object ImmutableIntMap {
  val empty: ImmutableIntMap = new ImmutableIntMap(Array.emptyLongArray)
}

/**
 * INTERNAL API
 * Specialized Map for primitive `Int` keys and values to avoid allocations (boxing).
 * Keys and values are encoded in a single Long array and does copy-on-write with no
 * structural sharing, it's intended for rather small maps (<1000 elements).
 */
@InternalApi private[akka] final class ImmutableIntMap private (private final val kvs: Array[Long]) {

  final def size: Int = kvs.length

  private[this] final def indexForKey(key: Int): Int = {
    @tailrec def find(lo: Int, hi: Int): Int =
      if (lo <= hi) {
        val at = (lo + hi) >>> 1
        val eKey = keyFromEntry(kvs(at))
        if (eKey < key) find(at + 1, hi)
        else if (eKey > key) find(lo, at - 1)
        else at
      } else -(lo + 1)
    find(0, size - 1)
  }

  private[this] final def entry(key: Int, value: Int): Long = (key.toLong << 32) | (value & 0xffffffffL)
  private[this] final def keyFromEntry(entry: Long): Int = (entry >> 32).toInt // get rid of value bits
  private[this] final def valueFromEntry(entry: Long): Int = entry.toInt //cast away key bits

  /**
   * Worst case `O(log n)`, allocation free.
   * Will return Int.MinValue if not found, so beware of storing Int.MinValues
   */
  final def get(key: Int): Int = {
    @tailrec def find(lo: Int, hi: Int): Int =
      if (lo <= hi) {
        val at = (lo + hi) >>> 1
        val e = kvs(at)
        val eKey = keyFromEntry(e)
        if (eKey < key) find(at + 1, hi)
        else if (eKey > key) find(lo, at - 1)
        else valueFromEntry(e)
      } else Int.MinValue // MinValue is equivalent of Not Found
    find(0, size - 1)
  }

  /**
   * Worst case `O(log n)`, allocation free.
   */
  final def contains(key: Int): Boolean = indexForKey(key) >= 0

  final def updateIfAbsent(key: Int, value: ⇒ Int): ImmutableIntMap =
    if (size == 0) new ImmutableIntMap(Array(entry(key, value)))
    else {
      val i = indexForKey(key)
      if (i >= 0) this
      else insert(key, value, i)
    }

  /**
   * Worst case `O(log n)`, creates new `ImmutableIntMap`
   * with the given key with the given value.
   */
  final def updated(key: Int, value: Int): ImmutableIntMap =
    if (size == 0) new ImmutableIntMap(Array(entry(key, value)))
    else {
      val i = indexForKey(key)
      if (i >= 0) update(key, value, i)
      else insert(key, value, i)
    }

  private[this] final def update(key: Int, value: Int, index: Int): ImmutableIntMap = {
    val newKvs = new Array[Long](size)
    System.arraycopy(kvs, 0, newKvs, 0, kvs.length)
    newKvs(index) = entry(key, value)
    new ImmutableIntMap(newKvs)
  }

  private[this] final def insert(key: Int, value: Int, index: Int): ImmutableIntMap = {
    // insert the entry at the right position—keep the array sorted
    val at = -(index + 1)
    val newKvs = new Array[Long](size + 1)
    System.arraycopy(kvs, 0, newKvs, 0, at)
    newKvs(at) = entry(key, value)
    System.arraycopy(kvs, at, newKvs, at + 1, kvs.length - at)
    new ImmutableIntMap(newKvs)
  }

  final def remove(key: Int): ImmutableIntMap = {
    val i = indexForKey(key)
    if (i >= 0) {
      if (size == 1)
        ImmutableIntMap.empty
      else {
        val newKvs = new Array[Long](size - 1)
        System.arraycopy(kvs, 0, newKvs, 0, i)
        System.arraycopy(kvs, i + 1, newKvs, i, size - i - 1)
        new ImmutableIntMap(newKvs)
      }
    } else
      this
  }

  /**
   * All keys
   */
  final def keysIterator: Iterator[Int] = kvs.iterator.map(keyFromEntry)

  override final def toString: String =
    kvs.iterator.map({
      e ⇒ s"${keyFromEntry(e)} -> ${valueFromEntry(e)}"
    }).mkString("ImmutableIntMap(", ", ", ")")

  override final def hashCode: Int = {
    var result = HashCode.SEED
    result = HashCode.hash(result, kvs)
    result
  }

  override final def equals(obj: Any): Boolean = obj match {
    case other: ImmutableIntMap ⇒
      if (other eq this) true
      else if (size != other.size) false
      else {
        @tailrec def check(i: Int): Boolean = {
          if (i < 0) true
          else if (kvs(i) == other.kvs(i)) check(i - 1)
          else false
        }
        check(size - 1)
      }
    case _ ⇒ false
  }
}
