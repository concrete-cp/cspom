package cspom.util;

import BitVector._

final class SmallBitVector(val word: Long) extends AnyVal with BitVector {
  def -(position: Int): BitVector = {
    val newWord = word & ~(1L << position)
    if (word == newWord) {
      this
    } else if (word == 0L) {
      BitVector.empty
    } else {
      new SmallBitVector(newWord)
    }
  }

  def apply(position: Int): Boolean = {
    position < WORD_SIZE && (word & (1L << position)) != 0L;
  }

  def prevSetBit(start: Int): Int = {
    val prev = if (start >= WORD_SIZE) {
      word
    } else {
      word & ~(MASK << start);
    }

    WORD_SIZE - java.lang.Long.numberOfLeadingZeros(prev) - 1
  }

  def lastSetBit: Int = {
    WORD_SIZE - java.lang.Long.numberOfLeadingZeros(word) - 1
  }

  def nextSetBit(start: Int): Int = {
    if (start >= WORD_SIZE) {
      -1
    } else {
      val next = word & (MASK << start);
      if (next == 0) {
        -1
      } else {
        java.lang.Long.numberOfTrailingZeros(next)
      }
    }
  }

  def clearFrom(from: Int): BitVector = {
    if (from >= WORD_SIZE) {
      this
    } else if (from <= 0) {
      BitVector.empty
    } else {
      new SmallBitVector(word & ~(MASK << from))
    }
  }

  def clearUntil(until: Int): BitVector = {
    if (until >= WORD_SIZE) {
      BitVector.empty
    } else if (until < 0) {
      this
    } else {
      new SmallBitVector(word & (MASK << until))
    }
  }

  //  override def equals(o: Any) = o match {
  //    case bv: BitVector => bv.getWord(0) == word && bv.nextSetBit(WORD_SIZE) == -1
  //    case _ => false
  //  }
  //
  //  override def hashCode = word.toInt

  def intersects(bv: BitVector, position: Int): Boolean = {
    (bv.getWord(0) & word) != 0;
  }

  def intersects(bv: BitVector): Int = {
    if (intersects(bv, 0)) 0 else -1;
  }

  def nbWords: Int = 1

  def getWords: Array[Long] = Array(word)

  def ^(bv: BitVector): BitVector = {
    bv.setWord(0, bv.getWord(0) ^ this.word)
  }

  def &(bv: BitVector): BitVector = {
    val newWord = (bv.getWord(0) & this.word) //& (MASK >>> -size)
    if (newWord == word) {
      this
    } else {
      new SmallBitVector(word)
    }
  }

  def |(bv: BitVector): BitVector = {
    val newWord = (bv.getWord(0) | this.word) //& (MASK >>> -size)
    bv.setWord(0, newWord)
  }

  def isEmpty: Boolean = word == 0L;

  def cardinality: Int = java.lang.Long.bitCount(word);

  def subsetOf(bv: BitVector): Boolean = {
    (word & ~bv.getWord(0)) == 0L;
  }

  def getWord(i: Int): Long = {
    if (i > 0)
      0L;
    else
      word;
  }

  def setWord(pos: Int, word: Long): BitVector = {
    if (pos == 0) {
      if (word == this.word) {
        this
      } else {
        new SmallBitVector(word)
      }
    } else {
      val array = new Array[Long](pos + 1)
      array(0) = this.word
      array(pos) = word
      new LargeBitVector(array)
    }
  }

  def filter(f: Int => Boolean): BitVector = {
    var newWord = 0L
    var i = nextSetBit(0)
    while (i >= 0) {
      if (f(i)) {
        newWord |= (1L << i)
      }
      i = nextSetBit(i + 1)
    }
    if (newWord == word) {
      this
    } else {
      new SmallBitVector(newWord)
    }
  }
}