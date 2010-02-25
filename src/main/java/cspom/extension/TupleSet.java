package cspom.extension;

import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

/**
 * Implements an hashtable of tuples, hopefully faster than by wrapping a tuple
 * in an object and using JDK's HashSet.
 * 
 * @author vion
 * 
 * @param <T>
 *            type of tuples
 */
public final class TupleSet<T> extends AbstractSet<T[]> {

    private static final int DEFAULT_HASHTABLE_SIZE = 16;

    private List<T[]>[] hashTable;

    private int size;

    private int hashTableSize;

    public TupleSet(final int nbTuples) {
        super();
        this.size = 0;
        hashTableSize = firstPrimeAfter(nbTuples * 4 / 3);
        hashTable = (List<T[]>[]) new List<?>[hashTableSize];
    }

    /**
     * @param tuple
     *            the tuple to seek
     * @return The index of the tuple in the hashtable
     */
    private static <T> int index(final T[] tuple, int hashTableSize) {
        return Arrays.hashCode(tuple) & hashTableSize - 1;
    }

    public TupleSet() {
        this(DEFAULT_HASHTABLE_SIZE);
    }

    private static int firstPrimeAfter(final int value) {
        int test = 1;
        while (test < value) {
            test <<= 1;
        }
        return test;
    }

    @Override
    public boolean contains(final Object obj) {
        final T[] tuple = (T[]) obj;
        final List<T[]> subList = hashTable[index(tuple, hashTableSize)];
        if (subList == null) {
            return false;
        }
        return containsTuple(tuple, subList);
    }

    private static <T> boolean containsTuple(T[] tuple, List<T[]> list) {
        for (T[] t : list) {
            if (Arrays.equals(t, tuple)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean remove(final Object obj) {
        final T[] tuple = (T[]) obj;
        final ListIterator<T[]> itr = hashTable[index(tuple, hashTableSize)]
                .listIterator();
        while (itr.hasNext()) {
            if (Arrays.equals(itr.next(), tuple)) {
                itr.remove();
                size--;
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean add(final T[] tuple) {
        if (size >= hashTable.length) {
            expand((hashTable.length * 3 + 1) / 2);
        }
        final int index = index(tuple, hashTableSize);

        if (hashTable[index] == null) {
            hashTable[index] = new LinkedList<T[]>();
        } else if (containsTuple(tuple, hashTable[index])) {
            return false;
        }

        hashTable[index].add(tuple);
        size++;
        return true;
    }

    private void expand(int newTableSize) {
        final List<T[]>[] newTable = (List<T[]>[]) new List<?>[newTableSize];

        for (T[] t : this) {
            final int index = index(t, newTableSize);
            if (newTable[index] == null) {
                newTable[index] = new LinkedList<T[]>();
            }

            newTable[index].add(t);
        }
        hashTable = newTable;
        hashTableSize = newTableSize;
    }

    @Override
    public Iterator<T[]> iterator() {
        return new HashSetIterator();
    }

    private class HashSetIterator implements Iterator<T[]> {

        private int pos;

        private ListIterator<T[]> itr;

        public HashSetIterator() {
            pos = 0;

            while (pos < hashTableSize
                    && (hashTable[pos] == null || hashTable[pos].isEmpty())) {
                pos++;
            }
            if (pos < hashTableSize) {
                itr = hashTable[pos].listIterator();
            }

        }

        @Override
        public boolean hasNext() {
            return pos < hashTableSize;
        }

        @Override
        public T[] next() {
            final T[] tuple = itr.next();

            if (!itr.hasNext()) {
                do {
                    pos++;
                } while (pos < hashTableSize
                        && (hashTable[pos] == null || hashTable[pos].isEmpty()));
                if (pos < hashTableSize) {
                    itr = hashTable[pos].listIterator();
                }
            }
            return tuple;
        }

        @Override
        public void remove() {
            if (itr == null) {
                throw new IllegalStateException();
            }
            itr.remove();
        }

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
}
