package cspom.extension;

import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Implements an hash table of tuples, hopefully faster than by wrapping a tuple
 * in an object and using JDK's HashSet.
 * 
 * @author vion
 * 
 * @param <T>
 *            type of tuples
 */
public final class TupleSet<T> extends AbstractSet<T[]> {

    /**
     * Default size for the hash table.
     */
    private static final int DEFAULT_HASHTABLE_SIZE = 32;

    /**
     * Load factor for the hash table. Hash table will get resized once the
     * number of elements reaches hashTableSize * LOAD_FACTOR.
     */
    private static final float LOAD_FACTOR = .75f;

    /**
     * Current threshold for resizing. Set to hashTableSize * LOAD_FACTOR.
     */
    private int threshold;

    /**
     * The actual hash table.
     */
    private Entry<T>[] hashTable;

    /**
     * Number of actual elements in the hash table.
     */
    private int size;

    /**
     * Size (number of cells) of the hash table. Always a factor of 2.
     */
    private int hashTableSize;

    /**
     * Constructs a new TupleSet with nbTuples initial capacity.
     * 
     * @param nbTuples
     *            The initial capacity of the TupleSet.
     */
    public TupleSet(final int nbTuples) {
        super();
        this.size = 0;
        hashTableSize = 1;
        while (hashTableSize < nbTuples) {
            hashTableSize <<= 1;
        }
        threshold = (int) (hashTableSize * LOAD_FACTOR);
        hashTable = (Entry<T>[]) new Entry<?>[hashTableSize];
    }

    /**
     * Constructs a new TupleSet with default initial capacity (32).
     */
    public TupleSet() {
        this(DEFAULT_HASHTABLE_SIZE);
    }

    /**
     * Constructs a new TupleSet initially containing all items in the given
     * Collection.
     * 
     * @param collection
     *            the collection whose elements are to be placed into this set.
     */
    public TupleSet(final Collection<T[]> collection) {
        this(collection.size());
        addAll(collection);
    }

    /**
     * @param hash
     *            hash of the tuple to seek
     * @param hashTableSize
     *            size of the hash table
     * @return The index of the tuple in the hash table
     */
    private static int indexFor(final int hash, final int hashTableSize) {
        return hash & (hashTableSize - 1);
    }

    @Override
    public boolean contains(final Object obj) {
        return containsTuple((T[]) obj);
    }

    /**
     * 
     * @param tuple
     *            a tuple
     * @return true iff this set contains the specified tuple.
     */
    public boolean containsTuple(final T[] tuple) {
        return containsTuple(hashTable[indexFor(Arrays.hashCode(tuple),
                hashTableSize)], tuple);
    }

    @Override
    public boolean add(final T[] tuple) {
        final int hash = Arrays.hashCode(tuple);
        final int index = indexFor(hash, hashTableSize);

        if (containsTuple(hashTable[index], tuple)) {
            return false;
        }

        hashTable[index] = new Entry<T>(tuple, hash, hashTable[index]);
        if (size++ >= threshold) {
            resize(hashTable.length * 2);
        }
        return true;
    }

    /**
     * Resizes the underlying hash table to the given table size.
     * 
     * @param newTableSize
     *            the new table size.
     */
    private void resize(final int newTableSize) {
        final Entry<T>[] newTable = (Entry<T>[]) new Entry<?>[newTableSize];

        transfer(newTable);
        hashTable = newTable;
        hashTableSize = newTableSize;
        threshold = (int) (newTableSize * LOAD_FACTOR);
    }

    /**
     * Transfers all entries from current table to newTable.
     * 
     * @param newTable
     *            the new table.
     */
    private void transfer(final Entry<T>[] newTable) {
        final Entry<T>[] src = hashTable;
        final int newCapacity = newTable.length;
        for (int j = src.length; --j >= 0;) {
            Entry<T> e = src[j];
            if (e != null) {
                src[j] = null;
                do {
                    final Entry<T> next = e.next;
                    final int i = indexFor(e.hash, newCapacity);
                    e.next = newTable[i];
                    newTable[i] = e;
                    e = next;
                } while (e != null);
            }
        }
    }

    @Override
    public Iterator<T[]> iterator() {
        return new HashSetIterator();
    }

    /**
     * @param <T>
     *            type of the elements in the tuple.
     * @param list
     *            a linked list
     * @param tuple
     *            a tuple
     * @return true iff the given linked list contains the given tuple.
     */
    private static <T> boolean containsTuple(final Entry<T> list,
            final T[] tuple) {
        for (Entry<T> e = list; e != null; e = e.next) {
            if (Arrays.equals(e.entry, tuple)) {
                return true;
            }
        }
        return false;
    }

    /**
     * An iterator that iterates over entries of the hash table.
     * 
     * @author vion
     * 
     */
    private final class HashSetIterator implements Iterator<T[]> {

        /**
         * The current index in the hash table.
         */
        private int index;

        /**
         * Next entry.
         */
        private Entry<T> next;

        /**
         * Constructs an iterator over the enclosing set.
         */
        private HashSetIterator() {
            if (size > 0) { // advance to first entry
                final Entry<T>[] t = hashTable;
                while (index < t.length && (next = t[index++]) == null) {
                    //
                }

            }
        }

        @Override
        public boolean hasNext() {
            return next != null;
        }

        @Override
        public T[] next() {
            final Entry<T> e = next;
            if (e == null) {
                throw new NoSuchElementException();
            }

            next = e.next;

            if (next == null) {
                final Entry<T>[] t = hashTable;
                while (index < t.length && (next = t[index++]) == null) {
                    //
                }
            }
            return e.entry;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

    }

    @Override
    public boolean remove(final Object obj) {
        final T[] tuple = (T[]) obj;
        final int index = indexFor(Arrays.hashCode(tuple), hashTableSize);
        Entry<T> prev = hashTable[index];
        if (Arrays.equals(prev.entry, tuple)) {
            hashTable[index] = prev.next;
            size--;
            return true;
        }

        for (Entry<T> e = prev.next; e != null; prev = e, e = e.next) {
            if (Arrays.equals(e.entry, tuple)) {
                prev.next = e.next;
                size--;
                return true;
            }
        }

        return false;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void clear() {
        Arrays.fill(hashTable, null);
        size = 0;
    }

    /**
     * Structure used to implement a simple linked list of tuples.
     * 
     * @author vion
     * 
     * @param <T>
     */
    private static final class Entry<T> {
        /**
         * The tuple.
         */
        private final T[] entry;

        /**
         * Next element in the linked list.
         */
        private Entry<T> next;

        /**
         * Hash of the tuple (cached).
         */
        private final int hash;

        /**
         * Constructs a new entry at the head of the list with given tuple and
         * hash.
         * 
         * @param entry
         *            The tuple
         * @param hash
         *            Hash of the tuple
         * @param next
         *            The element following the new one.
         */
        private Entry(final T[] entry, final int hash, final Entry<T> next) {
            this.entry = entry;
            this.hash = hash;
            this.next = next;
        }

    }
}
