package seqs

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestConcat(t *testing.T) {
	testCases := map[string]struct {
		seqs []Seq[int]
		want Seq[int]
	}{
		"no seqs": {
			seqs: []Seq[int]{},
			want: Empty[int](),
		},
		"one seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"two seqs": {
			seqs: []Seq[int]{
				FromValues(1, 2),
				FromValues(3, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"more seqs": {
			seqs: []Seq[int]{
				FromValues(1),
				FromValues(2),
				FromValues(3),
				FromValues(4),
			},
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Concat(testCase.seqs...)))
		})
	}
}

func TestCycle(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		take int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			take: 10,
			want: Empty[int](),
		},
		"one element": {
			seq:  FromValues(1),
			take: 4,
			want: RepeatN(1, 4),
		},
		"more elements": {
			seq:  FromValues(1, 2, 3, 4),
			take: 10,
			want: FromValues(1, 2, 3, 4, 1, 2, 3, 4, 1, 2),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Take(Cycle(testCase.seq), testCase.take)))
		})
	}
}

func TestEmpty(t *testing.T) {
	require.Empty(t, ToSlice(Empty[int]()))
	require.Implements(t, (*Lener)(nil), Empty[int]())
	require.Zero(t, Empty[int]().(Lener).Len())
}

func TestFilter(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			pred: func(i int) bool { return true },
			want: Empty[int](),
		},
		"every other": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return i%2 == 0 },
			want: FromValues(2, 4),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return true },
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return false },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Filter(testCase.seq, testCase.pred)))
		})
	}
}

func TestFlatten(t *testing.T) {
	testCases := map[string]struct {
		seqs Seq[Seq[int]]
		want Seq[int]
	}{
		"no seqs": {
			seqs: Empty[Seq[int]](),
			want: Empty[int](),
		},
		"one seq": {
			seqs: FromValues(
				FromValues(1, 2, 3, 4),
			),
			want: FromValues(1, 2, 3, 4),
		},
		"two seqs": {
			seqs: FromValues(
				FromValues(1, 2),
				FromValues(3, 4),
			),
			want: FromValues(1, 2, 3, 4),
		},
		"more seqs": {
			seqs: FromValues(
				FromValues(1),
				FromValues(2),
				FromValues(3),
				FromValues(4),
			),
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Flatten[int](testCase.seqs)))
		})
	}
}

func TestIntersperse(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		val  int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			val:  42,
			want: Empty[int](),
		},
		"one element seq": {
			seq:  FromValues(1),
			val:  42,
			want: FromValues(1),
		},
		"two element seq": {
			seq:  FromValues(1, 2),
			val:  42,
			want: FromValues(1, 42, 2),
		},
		"many element seq": {
			seq:  FromValues(1, 2, 3, 4),
			val:  42,
			want: FromValues(1, 42, 2, 42, 3, 42, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Intersperse(testCase.seq, testCase.val)))
		})
	}
}

func TestMap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		require.Equal(t, ToSlice(Empty[int]()), ToSlice(Map(Empty[int](), func(i int) int { return i })))
	})
	t.Run("times two", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(2, 4, 6, 8)), ToSlice(Map(FromValues(1, 2, 3, 4), func(i int) int { return i * 2 })))
	})
	t.Run("to string", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues("1", "2", "3", "4")), ToSlice(Map(FromValues(1, 2, 3, 4), func(i int) string { return fmt.Sprint(i) })))
	})
}

func TestReduce(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		require.Equal(t, 42, Reduce(Empty[int](), 42, func(a int, e int) int { return a + e }))
	})
	t.Run("offset sum", func(t *testing.T) {
		require.Equal(t, 20, Reduce(FromValues(1, 2, 3, 4), 10, func(a int, e int) int { return a + e }))
	})
}

func TestRepeat(t *testing.T) {
	require.Equal(t, ToSlice(RepeatN(42, 6)), ToSlice(Take(Repeat(42), 6)))
}

func TestRoundRobin(t *testing.T) {
	testCases := map[string]struct {
		seqs []Seq[int]
		want Seq[int]
	}{
		"no seqs": {
			seqs: []Seq[int]{},
			want: Empty[int](),
		},
		"one seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"two seqs": {
			seqs: []Seq[int]{
				FromValues(1, 3),
				FromValues(2, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"more seqs": {
			seqs: []Seq[int]{
				FromValues(1),
				FromValues(2, 2),
				FromValues(3, 3, 3),
				FromValues(4, 4, 4, 4),
			},
			want: FromValues(1, 2, 3, 4, 2, 3, 4, 3, 4, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(RoundRobin(testCase.seqs...)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 3, 4, 2, 3)), ToSlice(Take(RoundRobin(RepeatN(1, 1), RepeatN(2, 2), RepeatN(3, 3), RepeatN(4, 4)), 6)))
	})
}

func TestSkip(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		n    int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			n:    42,
			want: Empty[int](),
		},
		"skip 3": {
			seq:  FromValues(1, 2, 3, 4),
			n:    3,
			want: FromValues(4),
		},
		"skip more": {
			seq:  FromValues(1, 2, 3, 4),
			n:    42,
			want: Empty[int](),
		},
		"skip none": {
			seq:  FromValues(1, 2, 3, 4),
			n:    0,
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Skip(testCase.seq, testCase.n)))
		})
	}
}

func TestSkipWhile(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			pred: func(i int) bool { return false },
			want: Empty[int](),
		},
		"less than 3": {
			seq:  FromValues(1, 2, 3, 4, 3, 2, 1),
			pred: func(i int) bool { return i < 3 },
			want: FromValues(3, 4, 3, 2, 1),
		},
		"skip all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return true },
			want: Empty[int](),
		},
		"skip none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return false },
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(SkipWhile(testCase.seq, testCase.pred)))
		})
	}
}

func TestTake(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		n    int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			n:    42,
			want: Empty[int](),
		},
		"take 3": {
			seq:  FromValues(1, 2, 3, 4),
			n:    3,
			want: FromValues(1, 2, 3),
		},
		"take more": {
			seq:  FromValues(1, 2, 3, 4),
			n:    42,
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			n:    0,
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Take(testCase.seq, testCase.n)))
		})
	}
}

func TestTakeWhile(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			pred: func(i int) bool { return true },
			want: Empty[int](),
		},
		"less than 3": {
			seq:  FromValues(1, 2, 3, 4, 3, 2, 1),
			pred: func(i int) bool { return i < 3 },
			want: FromValues(1, 2),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return true },
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return false },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(TakeWhile(testCase.seq, testCase.pred)))
		})
	}
}

func TestToSet(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want map[int]bool
	}{
		"empty seq": {
			seq:  Empty[int](),
			want: map[int]bool{},
		},
		"unique values": {
			seq:  FromValues(1, 2, 3, 4),
			want: map[int]bool{1: true, 2: true, 3: true, 4: true},
		},
		"repeating values": {
			seq:  FromValues(1, 2, 3, 4, 3, 2, 1),
			want: map[int]bool{1: true, 2: true, 3: true, 4: true},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, testCase.want, ToSet(testCase.seq))
		})
	}
}
