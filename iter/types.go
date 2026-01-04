package iter

// MuxSeq2 represents a 2-way heterogeneous sequence of values.
type MuxSeq2[Item1, Item2 any] func(yield1 func(Item1) bool, yield2 func(Item2) bool)

// MuxSeq3 represents a 3-way heterogeneous sequence of values.
type MuxSeq3[Item1, Item2, Item3 any] func(yield1 func(Item1) bool, yield2 func(Item2) bool, yield3 func(Item3) bool)

// MuxSeq4 represents a 4-way heterogeneous sequence of values.
type MuxSeq4[Item1, Item2, Item3, Item4 any] func(yield1 func(Item1) bool, yield2 func(Item2) bool, yield3 func(Item3) bool, yield4 func(Item4) bool)
