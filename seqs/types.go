package seqs

// Seq defines a minimal interface for a sequence of items.
type Seq[Item any] interface {
	// ForEachWhile calls the specified function for each item in the sequence until it returns false.
	ForEachWhile(yield func(Item) bool)
}

// FiniteSeq is a [Seq] that has a finite length.
type FiniteSeq[Item any] interface {
	Seq[Item]
	// Len returns the length of the sequence
	//
	// This method returns an `int` to be consistent with the `len()` built-in function.
	Len() int
}

// Indexable is a minimal interface for collections whose items can be accessed directly using an integer index.
type Indexable[Item any] interface {
	// Index returns the ith item of the collection.
	// It should panic when the index is out of bounds.
	Index(i int) Item
}

// Pair is a minimal interface for a pair of (possibly different types of) values.
type Pair[Value1, Value2 any] interface {
	// Unpack returns the pair of values.
	Unpack() (Value1, Value2)
}
