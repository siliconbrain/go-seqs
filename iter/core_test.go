package iter

import (
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestAll(t *testing.T) {
	t.Run("should alwaye be true for empty seq", func(t *testing.T) {
		assert.True(t, All(Empty[int], func(int) bool { return false }))
	})
	t.Run("should return true when all items match", func(t *testing.T) {
		assert.True(t, All(FromValues(1, 2, 3), func(i int) bool { return i < 4 }))
	})
	t.Run("should return false when not all items match", func(t *testing.T) {
		assert.False(t, All(FromValues(1, 2, 3, 4, 3), func(i int) bool { return i < 4 }))
	})
	t.Run("should short circuit", func(t *testing.T) {
		assert.NotPanics(t, func() {
			seq := func(yield func(int) bool) {
				if !yield(1) {
					return
				}
				if !yield(2) {
					return
				}
				if !yield(3) {
					return
				}
				panic("should terminate before this")
			}
			_ = All(seq, func(i int) bool { return i < 3 })
		})
	})
}

func TestAll2(t *testing.T) {
	t.Run("should alwaye be true for empty seq", func(t *testing.T) {
		assert.True(t, All2(Empty2[int, string], func(int, string) bool { return false }))
	})
	t.Run("should return true when all pairs match", func(t *testing.T) {
		assert.True(t, All2(pairs[int, string]{{1, "a"}, {2, "aa"}, {3, "aaa"}}.All,
			func(i int, s string) bool { return i < 4 && strings.Contains(s, "a") }))
	})
	t.Run("should return false when not all pairs match", func(t *testing.T) {
		assert.False(t, All2(pairs[int, string]{{1, "a"}, {2, "aa"}, {3, "aaa"}, {4, "bbbb"}, {3, "aaa"}}.All,
			func(i int, s string) bool { return i < 4 && strings.Contains(s, "a") }))
	})
	t.Run("should short circuit", func(t *testing.T) {
		assert.NotPanics(t, func() {
			seq := func(yield func(int, string) bool) {
				if !yield(1, "a") {
					return
				}
				if !yield(2, "aa") {
					return
				}
				if !yield(3, "aaa") {
					return
				}
				panic("should terminate before this")
			}
			_ = All2(seq, func(i int, s string) bool { return i < 3 })
		})
	})
}

func TestAny(t *testing.T) {
	t.Run("should alwaye be false for empty seq", func(t *testing.T) {
		assert.False(t, Any(Empty[int], func(int) bool { return true }))
	})
	t.Run("should return true when any items match", func(t *testing.T) {
		assert.True(t, Any(FromValues(1, 2, 3), func(i int) bool { return i > 1 }))
	})
	t.Run("should return false when no items match", func(t *testing.T) {
		assert.False(t, Any(FromValues(1, 2, 3, 4, 3), func(i int) bool { return i > 4 }))
	})
	t.Run("should short circuit", func(t *testing.T) {
		assert.NotPanics(t, func() {
			seq := func(yield func(int) bool) {
				if !yield(1) {
					return
				}
				if !yield(2) {
					return
				}
				if !yield(3) {
					return
				}
				panic("should terminate before this")
			}
			_ = Any(seq, func(i int) bool { return i > 2 })
		})
	})
}

func TestAny2(t *testing.T) {
	t.Run("should alwaye be false for empty seq", func(t *testing.T) {
		assert.False(t, Any2(Empty2[int, string], func(int, string) bool { return true }))
	})
	t.Run("should return true when any pairs match", func(t *testing.T) {
		assert.True(t, Any2(pairs[int, string]{{1, "a"}, {2, "aa"}, {3, "aaa"}}.All, func(i int, s string) bool { return i > 1 }))
	})
	t.Run("should return false when no pairs match", func(t *testing.T) {
		assert.False(t, Any2(pairs[int, string]{{1, "b"}, {2, "aa"}, {3, "aaa"}}.All,
			func(i int, s string) bool { return i > 1 && strings.Contains(s, "b") }))
	})
	t.Run("should short circuit", func(t *testing.T) {
		assert.NotPanics(t, func() {
			seq := func(yield func(int, string) bool) {
				if !yield(1, "a") {
					return
				}
				if !yield(2, "aa") {
					return
				}
				if !yield(3, "aaa") {
					return
				}
				panic("should terminate before this")
			}
			_ = Any2(seq, func(i int, s string) bool { return i > 2 })
		})
	})
}

func TestBimap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Bimap(Empty2[int, string],
			func(i int) time.Duration { return time.Duration(i) },
			func(s string) []byte { return []byte(s) },
		)))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{2, "one+1"}, {3, "two+1"}}, collect2(Bimap(
			pairs[int, string]{{1, "one"}, {2, "two"}}.All,
			func(i int) int { return i + 1 },
			func(s string) string { return s + "+1" },
		)))
	})

	t.Run("break early", func(t *testing.T) {
		for range Bimap(pairs[int, string]{{1, "one"}, {2, "two"}}.All, id, id) {
			break
		}
	})
}

func TestCartesian(t *testing.T) {
	t.Run("empty x empty", func(t *testing.T) {
		assert.Empty(t, collect2(Cartesian(Empty[int], Empty[string])))
	})

	t.Run("empty x non-empty", func(t *testing.T) {
		assert.Empty(t, collect2(Cartesian(Empty[int], FromValues("one", "two"))))
	})

	t.Run("non-empty x empty", func(t *testing.T) {
		assert.Empty(t, collect2(Cartesian(FromValues(1, 2), Empty[string])))
	})

	t.Run("non-empty x non-empty", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "one"}, {1, "two"}, {2, "one"}, {2, "two"}},
			collect2(Cartesian(FromValues(1, 2), FromValues("one", "two"))))
	})
}

func TestConcat(t *testing.T) {
	t.Run("no seqs", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Concat[int]()))
	})

	t.Run("empty seqs", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Concat(Empty[int], Empty[int])))
	})

	t.Run("non-empty seqs", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4, 5, 6}, slices.Collect(Concat(
			FromValues(1, 2, 3),
			FromValues(4, 5, 6),
		)))
	})

	t.Run("mixed seqs", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 2, 3, 3, 4}, slices.Collect(Concat(
			Empty[int],
			FromValues(1, 2),
			Empty[int],
			FromValues(2, 3),
			Empty[int],
			FromValues(3, 4),
			Empty[int],
		)))
	})
}

func TestConcat2(t *testing.T) {
	t.Run("no seqs", func(t *testing.T) {
		assert.Empty(t, collect2(Concat2[int, string]()))
	})

	t.Run("empty seqs", func(t *testing.T) {
		assert.Empty(t, collect2(Concat2(Empty2[int, string], Empty2[int, string])))
	})

	t.Run("non-empty seqs", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}, collect2(Concat2(
			pairs[int, string]{{1, "a"}}.All,
			pairs[int, string]{{2, "b"}}.All,
			pairs[int, string]{{3, "c"}}.All,
		)))
	})
}

func TestCount(t *testing.T) {
	t.Run("from 0 with step 1", func(t *testing.T) {
		assert.Equal(t, []int{0, 1, 2, 3}, slices.Collect(Take(Count(0, 1), 4)))
	})
	t.Run("from 42 with step 0", func(t *testing.T) {
		assert.Equal(t, []int{42, 42, 42, 42}, slices.Collect(Take(Count(42, 0), 4)))
	})
	t.Run("from 42 with step -1", func(t *testing.T) {
		assert.Equal(t, []int{42, 41, 40, 39}, slices.Collect(Take(Count(42, -1), 4)))
	})
	t.Run("strings", func(t *testing.T) {
		assert.Equal(t, []string{"abc", "abcde", "abcdede", "abcdedede"}, slices.Collect(Take(Count("abc", "de"), 4)))
	})
}

func TestCycle(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Cycle(Empty[int])))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 1, 2, 3, 1, 2}, slices.Collect(Take(Cycle(FromValues(1, 2, 3)), 8)))
	})
}

func TestCycle2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Cycle2(Empty2[int, string])))
	})
	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{
			{1, "a"},
			{2, "b"},
			{3, "c"},
			{1, "a"},
		}, collect2(Take2(Cycle2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All), 4)))
	})
}

func TestDivvy(t *testing.T) {
	t.Run("zero count", func(t *testing.T) {
		assert.Panics(t, func() {
			Divvy(FromValues(1, 2, 3, 4), 0, 1)
		})
	})
	t.Run("zero skip", func(t *testing.T) {
		assert.Panics(t, func() {
			Divvy(FromValues(1, 2, 3, 4), 1, 0)
		})
	})
	testCases := map[string]struct {
		seq   Seq[int]
		count int
		skip  int
		want  [][]int
	}{
		"empty seq": {
			seq:   Empty[int],
			count: 1,
			skip:  1,
			want:  [][]int(nil),
		},
		"each element by itself": {
			seq:   FromValues(1, 2, 3, 4),
			count: 1,
			skip:  1,
			want:  [][]int{{1}, {2}, {3}, {4}},
		},
		"overlapping pairs": {
			seq:   FromValues(1, 2, 3, 4),
			count: 2,
			skip:  1,
			want:  [][]int{{1, 2}, {2, 3}, {3, 4}},
		},
		"adjacent pairs": {
			seq:   FromValues(1, 2, 3, 4),
			count: 2,
			skip:  2,
			want:  [][]int{{1, 2}, {3, 4}},
		},
		"disjunct pairs": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7, 8),
			count: 2,
			skip:  4,
			want:  [][]int{{1, 2}, {5, 6}},
		},
		"overlapping triplets": {
			seq:   FromValues(1, 2, 3, 4),
			count: 3,
			skip:  1,
			want:  [][]int{{1, 2, 3}, {2, 3, 4}},
		},
		"adjacent triplets": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7),
			count: 3,
			skip:  3,
			want:  [][]int{{1, 2, 3}, {4, 5, 6}, {7}},
		},
		"disjunct triplets": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7),
			count: 3,
			skip:  4,
			want:  [][]int{{1, 2, 3}, {5, 6, 7}},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, slices.Collect(Divvy(testCase.seq, testCase.count, testCase.skip)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, [][]int{{1, 2}, {2, 3}}, slices.Collect(Take(Divvy(FromValues(1, 2, 3, 4, 5, 6), 2, 1), 2)))
	})
}

func TestDivvyExact(t *testing.T) {
	t.Run("zero count", func(t *testing.T) {
		assert.Panics(t, func() {
			DivvyExact(FromValues(1, 2, 3, 4), 0, 1)
		})
	})
	t.Run("zero skip", func(t *testing.T) {
		assert.Panics(t, func() {
			DivvyExact(FromValues(1, 2, 3, 4), 1, 0)
		})
	})
	testCases := map[string]struct {
		seq   Seq[int]
		count int
		skip  int
		want  [][]int
	}{
		"empty seq": {
			seq:   Empty[int],
			count: 1,
			skip:  1,
			want:  [][]int(nil),
		},
		"each element by itself": {
			seq:   FromValues(1, 2, 3, 4),
			count: 1,
			skip:  1,
			want:  [][]int{{1}, {2}, {3}, {4}},
		},
		"overlapping pairs": {
			seq:   FromValues(1, 2, 3, 4),
			count: 2,
			skip:  1,
			want:  [][]int{{1, 2}, {2, 3}, {3, 4}},
		},
		"adjacent pairs": {
			seq:   FromValues(1, 2, 3, 4),
			count: 2,
			skip:  2,
			want:  [][]int{{1, 2}, {3, 4}},
		},
		"disjunct pairs": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7, 8),
			count: 2,
			skip:  4,
			want:  [][]int{{1, 2}, {5, 6}},
		},
		"overlapping triplets": {
			seq:   FromValues(1, 2, 3, 4),
			count: 3,
			skip:  1,
			want:  [][]int{{1, 2, 3}, {2, 3, 4}},
		},
		"adjacent triplets": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7),
			count: 3,
			skip:  3,
			want:  [][]int{{1, 2, 3}, {4, 5, 6}},
		},
		"disjunct triplets": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7),
			count: 3,
			skip:  4,
			want:  [][]int{{1, 2, 3}, {5, 6, 7}},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, slices.Collect(DivvyExact(testCase.seq, testCase.count, testCase.skip)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, [][]int{{1, 2}, {2, 3}}, slices.Collect(Take(DivvyExact(FromValues(1, 2, 3, 4, 5, 6), 2, 1), 2)))
	})
}

func TestEmpty(t *testing.T) {
	assert.Empty(t, slices.Collect(Empty[int]))
}

func TestEmpty2(t *testing.T) {
	assert.Empty(t, collect2(Empty2[int, string]))
}

func TestEnumerate(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Enumerate(Empty[string])))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{0, "a"}, {1, "b"}, {2, "c"}}, collect2(Enumerate(FromValues("a", "b", "c"))))
	})
}

func TestFilter(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Filter(Empty[int], func(int) bool { return true })))
	})

	t.Run("non-empty seq, const false pred", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Filter(FromValues("a", "b", "c"), func(string) bool { return false })))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []int{1, 3}, slices.Collect(Filter(FromValues(1, 2, 3, 4), func(v int) bool { return v%2 == 1 })))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{1}, slices.Collect(Take(Filter(
			FromValues(1, 2, 3, 4),
			func(v int) bool { return v%2 == 1 },
		), 1)))
	})
}

func TestFilter2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Filter2(Empty2[int, string], func(int, string) bool { return true })))
	})

	t.Run("non-empty seq, const false pred", func(t *testing.T) {
		assert.Empty(t, collect2(Filter2(
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All,
			func(int, string) bool { return false },
		)))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {3, "c"}}, collect2(Filter2(
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}}.All,
			func(v int, _ string) bool { return v%2 == 1 },
		)))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}}, collect2(Take2(Filter2(
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}}.All,
			func(v int, _ string) bool { return v%2 == 1 },
		), 1)))
	})
}

func TestFilterMap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(FilterMap(Empty[int], func(v int) (float64, bool) { return float64(v), true })))
	})

	t.Run("non-empty seq, const false pred", func(t *testing.T) {
		assert.Empty(t, slices.Collect(FilterMap(
			FromValues(1, 2, 3, 4),
			func(v int) (float64, bool) { return float64(v), false },
		)))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []float64{1, 3}, slices.Collect(FilterMap(
			FromValues(1, 2, 3, 4),
			func(v int) (float64, bool) { return float64(v), v%2 == 1 },
		)))
	})
}

func TestFilterMap2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(FilterMap2(Empty2[int, string], func(v int, _ string) (float64, byte, bool) { return float64(v), 0, true })))
	})

	t.Run("non-empty seq, const false pred", func(t *testing.T) {
		assert.Empty(t, collect2(FilterMap2(
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All,
			func(v int, _ string) (float64, byte, bool) { return float64(v), 0, false },
		)))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[float64, byte]{{1, 'a'}, {3, 'c'}}, collect2(FilterMap2(
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All,
			func(v int, s string) (float64, byte, bool) { return float64(v), s[0], v%2 == 1 },
		)))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[float64, byte]{{1, 'a'}}, collect2(Take2(FilterMap2(
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All,
			func(v int, s string) (float64, byte, bool) { return float64(v), s[0], v%2 == 1 },
		), 1)))
	})
}

func TestFirst(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		_, exists := First(Empty[int])
		assert.False(t, exists)
	})

	t.Run("non-empty seq", func(t *testing.T) {
		val, exists := First(FromValues(1, 2, 3))
		assert.True(t, exists)
		assert.Equal(t, val, 1)
	})
}

func TestFirst2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		_, _, exists := First2(Empty2[int, string])
		assert.False(t, exists)
	})

	t.Run("non-empty seq", func(t *testing.T) {
		val1, val2, exists := First2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All)
		assert.True(t, exists)
		assert.Equal(t, val1, 1)
		assert.Equal(t, val2, "a")
	})
}

func TestFlatten(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Flatten(Empty[Seq[int]])))
	})

	t.Run("non-empty seq with empty seqs", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4, 5, 6},
			slices.Collect(Flatten(FromValues(FromValues(1, 2, 3), Empty[int], FromValues(4, 5, 6)))))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4, 5, 6},
			slices.Collect(Take(Flatten(FromValues(FromValues(1, 2, 3), Count(4, 1))), 6)))
	})
}

func TestFlatten2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Flatten2(Empty[Seq2[int, string]])))
	})

	t.Run("non-empty seq with empty seqs", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{0, "a"}, {1, "b"}, {0, "c"}, {1, "d"}},
			collect2(Flatten2(FromValues(
				Enumerate(FromValues("a", "b")),
				Empty2[int, string],
				Enumerate(FromValues("c", "d")),
			))))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{0, "a"}, {1, "b"}, {0, ""}, {1, "c"}},
			collect2(Take2(Flatten2(FromValues(
				Enumerate(FromValues("a", "b")),
				Enumerate(Count("", "c")),
			)), 4)))
	})
}

func TestFold(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Equal(t, 42, Fold(
			Empty[string], 42, func(acc int, str string) int { return acc + len(str) },
		))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, 45, Fold(
			FromValues("", "a", "aa"), 42, func(acc int, str string) int { return acc + len(str) },
		))
	})
}

func TestFold2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Equal(t, 42, Fold2(
			Empty2[int, string], 42, func(acc int, val int, str string) int { return acc + val*len(str) },
		))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, 47, Fold2(
			Enumerate(FromValues("", "a", "aa")), 42, func(acc int, val int, str string) int { return acc + val*len(str) },
		))
	})
}

func TestFolds(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Equal(t, []int{42}, slices.Collect(Folds(
			Empty[float64], 42, func(acc int, itm float64) int { return acc + int(itm) },
		)))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []int{42, 43, 45, 48}, slices.Collect(Folds(
			FromValues(1, 2, 3), 42, func(acc, val int) int { return acc + val },
		)))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{42, 43, 45, 48}, slices.Collect(Take(Folds(
			Count(1, 1), 42, func(acc, val int) int { return acc + val },
		), 4)))
	})

	t.Run("break even earlier", func(t *testing.T) {
		assert.Equal(t, []int{42}, slices.Collect(Take(Folds(
			Count(1, 1), 42, func(acc, val int) int { return acc + val },
		), 1)))
	})
}

func TestFolds2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Equal(t, []int{42}, slices.Collect(Folds2(
			Empty2[int, float64], 42, func(acc int, mul int, val float64) int { return acc + mul*int(val) },
		)))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []int{42, 42, 44, 50}, slices.Collect(Folds2(
			Enumerate(FromValues(1.0, 2, 3)), 42, func(acc int, mul int, val float64) int { return acc + mul*int(val) },
		)))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{42, 42, 44, 50}, slices.Collect(Take(Folds2(
			Enumerate(Count(1.0, 1.0)), 42, func(acc int, mul int, val float64) int { return acc + mul*int(val) },
		), 4)))
	})

	t.Run("break even earlier", func(t *testing.T) {
		assert.Equal(t, []int{42}, slices.Collect(Take(Folds2(
			Enumerate(Count(1.0, 1.0)), 42, func(acc int, mul int, val float64) int { return acc + mul*int(val) },
		), 1)))
	})
}

func TestInterleave(t *testing.T) {
	t.Run("no seqs", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Interleave[int]()))
	})
	t.Run("empty seqs", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Interleave(Empty[int], Empty[int])))
	})
	t.Run("non-empty with empty seqs", func(t *testing.T) {
		assert.Equal(t, []int{1},
			slices.Collect(Interleave(FromValues(1, 3), Empty[int], FromValues(2, 4))))
	})
	t.Run("different length non-empty seqs", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4, 5, 6, 7},
			slices.Collect(Interleave(Count(1, 3), FromValues(2, 5), FromValues(3, 6, 9))))
	})
	t.Run("infinite seqs", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 1, 2, 3, 1, 2, 3},
			slices.Collect(Take(Interleave(Repeat(1), Repeat(2), Repeat(3)), 9)))
	})
}

func TestInterleave2(t *testing.T) {
	t.Run("no seqs", func(t *testing.T) {
		assert.Empty(t, collect2(Interleave2[int, string]()))
	})
	t.Run("empty seqs", func(t *testing.T) {
		assert.Empty(t, collect2(Interleave2(Empty2[int, string], Empty2[int, string])))
	})
	t.Run("non-empty with empty seqs", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}}, collect2(Interleave2(
			pairs[int, string]{{1, "a"}, {3, "c"}}.All,
			Empty2[int, string],
			pairs[int, string]{{2, "b"}, {4, "d"}}.All,
		)))
	})
	t.Run("different length non-empty seqs", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{
			{0, ""}, {1, "a"}, {2, "b"},
			{0, ""}, {3, "c"}, {4, "d"},
			{0, ""},
		}, collect2(Interleave2(
			Repeat2(0, ""),
			pairs[int, string]{{1, "a"}, {3, "c"}}.All,
			pairs[int, string]{{2, "b"}, {4, "d"}, {6, "f"}}.All,
		)))
	})
	t.Run("infinite seqs", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{
			{1, "a"}, {2, "b"}, {3, "c"},
			{1, "a"}, {2, "b"}, {3, "c"},
			{1, "a"}, {2, "b"}, {3, "c"},
		}, collect2(Take2(Interleave2(
			Repeat2(1, "a"), Repeat2(2, "b"), Repeat2(3, "c"),
		), 9)))
	})
}

func TestIsEmpty(t *testing.T) {
	t.Run("should return true for empty seq", func(t *testing.T) {
		assert.True(t, IsEmpty(Empty[int]))
	})
	t.Run("should return false for non-empty seq", func(t *testing.T) {
		assert.False(t, IsEmpty(FromValues(1, 2, 3)))
	})
	t.Run("should work with infinite seq", func(t *testing.T) {
		assert.False(t, IsEmpty(Repeat(1)))
	})
}

func TestIsEmpty2(t *testing.T) {
	t.Run("should return true for empty seq", func(t *testing.T) {
		assert.True(t, IsEmpty2(Empty2[int, string]))
	})
	t.Run("should return false for non-empty seq", func(t *testing.T) {
		assert.False(t, IsEmpty2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All))
	})
	t.Run("should work with infinite seq", func(t *testing.T) {
		assert.False(t, IsEmpty2(Repeat2(1, "a")))
	})
}

func TestLast(t *testing.T) {
	t.Run("should return zero and false for empty seq", func(t *testing.T) {
		last, exists := Last(Empty[int])
		assert.Zero(t, last)
		assert.False(t, exists)
	})
	t.Run("should return last item and true for non-empty seq", func(t *testing.T) {
		last, exists := Last(Take(Count(1, 1), 42))
		assert.Equal(t, 42, last)
		assert.True(t, exists)
	})
}

func TestLast2(t *testing.T) {
	t.Run("should return zero and false for empty seq", func(t *testing.T) {
		last1, last2, exists := Last2(Empty2[int, string])
		assert.Zero(t, last1)
		assert.Zero(t, last2)
		assert.False(t, exists)
	})
	t.Run("should return last pair and true for non-empty seq", func(t *testing.T) {
		last1, last2, exists := Last2(Take2(Enumerate(Count(1, 1)), 42))
		assert.Equal(t, 41, last1)
		assert.Equal(t, 42, last2)
		assert.True(t, exists)
	})
}

func TestLen(t *testing.T) {
	t.Run("should return 0 for empty seq", func(t *testing.T) {
		assert.Equal(t, 0, Len(Empty[int]))
	})
	t.Run("should return number of items for non-empty seq", func(t *testing.T) {
		assert.Equal(t, 42, Len(RepeatN(0, 42)))
	})
}

func TestLen2(t *testing.T) {
	t.Run("should return 0 for empty seq", func(t *testing.T) {
		assert.Equal(t, 0, Len2(Empty2[int, string]))
	})
	t.Run("should return number of pairs for non-empty seq", func(t *testing.T) {
		assert.Equal(t, 42, Len2(RepeatN2(0, "a", 42)))
	})
}

func TestMap(t *testing.T) {
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4}, slices.Collect(Take(Map(Count(0, 1), func(v int) int { return v + 1 }), 4)))
	})
}

func TestMap2(t *testing.T) {
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {2, "b"}},
			collect2(Take2(Map2(Enumerate(FromValues("a", "b", "c", "d")), func(v int, s string) (int, string) { return v + 1, s }), 2)))
	})
}

func TestMemoize(t *testing.T) {
	makeSingleShot := func(t *testing.T, seq Seq[int]) Seq[int] {
		forced := false
		return func(yield func(int) bool) {
			if forced {
				assert.FailNow(t, "underlying sequence must only be enumerated once")
			}
			forced = true
			seq(yield)
		}
	}

	t.Run("finite seq", func(t *testing.T) {
		memoSeq := Memoize(makeSingleShot(t, FromValues(1, 2, 3, 4)))
		assert.Equal(t, []int{1, 2, 3, 4}, slices.Collect(memoSeq))
		assert.Equal(t, []int{1, 2, 3, 4}, slices.Collect(memoSeq))
	})
	t.Run("infinite seq", func(t *testing.T) {
		memoSeq := Memoize(makeSingleShot(t, Count(1, 1)))
		assert.Equal(t, []int{1, 2, 3, 4}, slices.Collect(Take(memoSeq, 4)))
		assert.Equal(t, []int{1, 2, 3, 4, 5, 6}, slices.Collect(Take(memoSeq, 6)))
	})
}

func TestMemoize2(t *testing.T) {
	makeSingleShot := func(t *testing.T, seq Seq2[int, string]) Seq2[int, string] {
		forced := false
		return func(yield func(int, string) bool) {
			if forced {
				assert.FailNow(t, "underlying sequence must only be enumerated once")
			}
			forced = true
			seq(yield)
		}
	}

	t.Run("finite seq", func(t *testing.T) {
		memoSeq := Memoize2(makeSingleShot(t, Enumerate(FromValues("a", "b", "c", "d"))))
		assert.Equal(t, pairs[int, string]{{0, "a"}, {1, "b"}, {2, "c"}, {3, "d"}}, collect2(memoSeq))
		assert.Equal(t, pairs[int, string]{{0, "a"}, {1, "b"}, {2, "c"}, {3, "d"}}, collect2(memoSeq))
	})
	t.Run("infinite seq", func(t *testing.T) {
		memoSeq := Memoize2(makeSingleShot(t, Enumerate(Count("", "a"))))
		assert.Equal(t, pairs[int, string]{{0, ""}, {1, "a"}, {2, "aa"}, {3, "aaa"}},
			collect2(Take2(memoSeq, 4)))
		assert.Equal(t, pairs[int, string]{{0, ""}, {1, "a"}, {2, "aa"}, {3, "aaa"}, {4, "aaaa"}, {5, "aaaaa"}},
			collect2(Take2(memoSeq, 6)))
	})
}

func TestPackMap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(PackMap(Empty2[[]rune, []byte],
			func(rs []rune, bs []byte) string { return string(rs) + string(bs) })))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []string{"one", "two"}, slices.Collect(PackMap(
			pairs[rune, []byte]{{'o', []byte("ne")}, {'t', []byte("wo")}}.All,
			func(r rune, bs []byte) string { return string(r) + string(bs) },
		)))
	})

	t.Run("break early", func(t *testing.T) {
		for range PackMap(pairs[int, int]{{1, 3}, {2, 4}}.All, func(a, b int) int { return a + b }) {
			break
		}
	})
}

func TestPullMany2(t *testing.T) {
	t.Run("no seqs", func(t *testing.T) {
		next, stop := PullMany2[int, string]()

		items1, items2, ok := next()
		assert.False(t, ok)
		assert.Zero(t, items1)
		assert.Zero(t, items2)

		assert.NotPanics(t, stop)
	})
	t.Run("empty seq", func(t *testing.T) {
		next, stop := PullMany2(Empty2[int, string])

		items1, items2, ok := next()
		assert.False(t, ok)
		assert.Zero(t, items1)
		assert.Zero(t, items2)

		assert.NotPanics(t, stop)
	})
	t.Run("different length seqs", func(t *testing.T) {
		next, stop := PullMany2(
			pairs[int, string]{{1, "a"}, {2, "b"}}.All,
			Empty2[int, string],
			pairs[int, string]{{3, "c"}}.All,
		)

		items1, items2, ok := next()
		assert.False(t, ok)
		assert.Zero(t, items1)
		assert.Zero(t, items2)

		assert.NotPanics(t, stop)
	})
	t.Run("different length non-empty seqs", func(t *testing.T) {
		next, stop := PullMany2(
			pairs[int, string]{{1, "a"}, {2, "b"}}.All,
			pairs[int, string]{{3, "c"}}.All,
		)

		items1, items2, ok := next()
		assert.True(t, ok)
		assert.Equal(t, []int{1, 3}, items1)
		assert.Equal(t, []string{"a", "c"}, items2)

		assert.NotPanics(t, stop)
	})
}

func TestReduce(t *testing.T) {
	add := func(a, b int) int { return a + b }
	t.Run("empty seq", func(t *testing.T) {
		assert.Zero(t, Reduce(Empty[int], add))
	})
	t.Run("singleton seq", func(t *testing.T) {
		assert.Equal(t, 42, Reduce(FromValues(42), add))
	})
	t.Run("multi-item seq", func(t *testing.T) {
		assert.Equal(t, 15, Reduce(FromValues(1, 2, 3, 4, 5), add))
	})
}

func TestReduce2(t *testing.T) {
	add := func(a int, s string, b int, t string) (int, string) { return a + b, s + t }
	t.Run("empty seq", func(t *testing.T) {
		res1, res2 := Reduce2(Empty2[int, string], add)
		assert.Zero(t, res1)
		assert.Zero(t, res2)
	})
	t.Run("singleton seq", func(t *testing.T) {
		res1, res2 := Reduce2(pairs[int, string]{{42, "fourty-two"}}.All, add)
		assert.Equal(t, 42, res1)
		assert.Equal(t, "fourty-two", res2)
	})
	t.Run("multi-item seq", func(t *testing.T) {
		res1, res2 := Reduce2(Take2(Zip(Count(1, 1), Count("a", "a")), 5), add)
		assert.Equal(t, 15, res1)
		assert.Equal(t, "aaaaaaaaaaaaaaa", res2)
	})
}

func TestReductions(t *testing.T) {
	add := func(a, b int) int { return a + b }
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Reductions(Empty[int], add)))
	})
	t.Run("singleton seq", func(t *testing.T) {
		assert.Equal(t, []int{42}, slices.Collect(Reductions(FromValues(42), add)))
	})
	t.Run("multi-item seq", func(t *testing.T) {
		assert.Equal(t, []int{1, 3, 6, 10, 15}, slices.Collect(Reductions(FromValues(1, 2, 3, 4, 5), add)))
	})
	t.Run("infinite seq", func(t *testing.T) {
		assert.Equal(t, []int{1, 3, 6, 10, 15}, slices.Collect(Take(Reductions(Count(1, 1), add), 5)))
	})
}

func TestReductions2(t *testing.T) {
	add := func(a int, s string, b int, t string) (int, string) { return a + b, s + t }
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Reductions2(Empty2[int, string], add)))
	})
	t.Run("singleton seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{42, "fourty-two"}}, collect2(Reductions2(pairs[int, string]{{42, "fourty-two"}}.All, add)))
	})
	t.Run("multi-pair seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {3, "abb"}, {6, "abbccc"}, {10, "abbcccdddd"}, {15, "abbcccddddeeeee"}},
			collect2(Reductions2(
				UnpackMap(FromValues("a", "bb", "ccc", "dddd", "eeeee"), func(s string) (int, string) { return len(s), s }),
				add,
			)),
		)
	})
	t.Run("infinite seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {3, "aaa"}, {6, "aaaaaa"}, {10, "aaaaaaaaaa"}, {15, "aaaaaaaaaaaaaaa"}},
			collect2(Take2(Reductions2(UnpackMap(Count("a", "a"), func(s string) (int, string) { return len(s), s }), add), 5)))
	})
}

func TestRepeat(t *testing.T) {
	assert.Equal(t, []int{1, 1, 1, 1}, slices.Collect(Take(Repeat(1), 4)))
}

func TestRepeat2(t *testing.T) {
	assert.Equal(t, pairs[int, string]{{1, "a"}, {1, "a"}, {1, "a"}, {1, "a"}},
		collect2(Take2(Repeat2(1, "a"), 4)))
}

func TestSkip(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Skip(Empty[int], 0)))
	})
	t.Run("non-empty seq, skip all", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Skip(FromValues(1, 2, 3), 4)))
	})
	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, []int{3}, slices.Collect(Skip(FromValues(1, 2, 3), 2)))
	})
	t.Run("non-empty seq, skip none", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3}, slices.Collect(Skip(FromValues(1, 2, 3), 0)))
	})
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{2}, slices.Collect(Take(Skip(FromValues(1, 2, 3), 1), 1)))
	})
}

func TestSkip2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Skip2(Empty2[int, string], 0)))
	})
	t.Run("non-empty seq, skip all", func(t *testing.T) {
		assert.Empty(t, collect2(Skip2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All, 4)))
	})
	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{3, "c"}}, collect2(Skip2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All, 2)))
	})
	t.Run("non-empty seq, skip none", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}},
			collect2(Skip2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All, 0)))
	})
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{2, "b"}},
			collect2(Take2(Skip2(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All, 1), 1)))
	})
}

func TestSkipWhile(t *testing.T) {
	t.Run("should return empty seq for empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(SkipWhile(Empty[int], func(int) bool { return false })))
	})
	t.Run("should return empty seq for const true pred", func(t *testing.T) {
		assert.Empty(t, slices.Collect(SkipWhile(RepeatN(42, 7), func(int) bool { return true })))
	})
	t.Run("should return tail of non-empty seq not matching pred", func(t *testing.T) {
		assert.Equal(t, []int{4, 5}, slices.Collect(SkipWhile(FromValues(1, 2, 3, 4, 5), func(v int) bool { return v < 4 })))
	})
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{4, 5, 6}, slices.Collect(Take(SkipWhile(Count(1, 1), func(v int) bool { return v < 4 }), 3)))
	})
}

func TestSkipWhile2(t *testing.T) {
	t.Run("should return empty seq for empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(SkipWhile2(Empty2[int, string], func(int, string) bool { return false })))
	})
	t.Run("should return empty seq for const true pred", func(t *testing.T) {
		assert.Empty(t, collect2(SkipWhile2(RepeatN2(42, "fourty-two", 7), func(int, string) bool { return true })))
	})
	t.Run("should return tail of non-empty seq not matching pred", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{3, "ccc"}, {4, "dddd"}, {5, "eeeee"}},
			collect2(SkipWhile2(
				UnpackMap(FromValues("a", "bb", "ccc", "dddd", "eeeee"), func(s string) (int, string) { return len(s), s }),
				func(v int, s string) bool { return v < 4 && len(s) < 3 },
			)),
		)
	})
	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{3, "aaa"}, {4, "aaaa"}}, collect2(Take2(SkipWhile2(
			UnpackMap(Count("a", "a"), func(s string) (int, string) { return len(s), s }),
			func(v int, s string) bool { return v < 3 },
		), 2)))
	})
}

func TestSum(t *testing.T) {
	t.Run("should return zero for empty seq", func(t *testing.T) {
		assert.Zero(t, Sum(Empty[string]))
	})
	t.Run("should return single item of singleton seq", func(t *testing.T) {
		assert.Equal(t, 42, Sum(FromValues(42)))
	})
	t.Run("should return sum of items for multi-item seq", func(t *testing.T) {
		assert.Equal(t, 15, Sum(FromValues(1, 2, 3, 4, 5)))
	})
}

func TestSums(t *testing.T) {
	t.Run("should return empty seq for empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Sums(Empty[int])))
	})
	t.Run("should return singleton seq for singleton seq", func(t *testing.T) {
		assert.Equal(t, []int{42}, slices.Collect(Sums(FromValues(42))))
	})
	t.Run("should return seq of partial sums for multi-item seq", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4, 5}, slices.Collect(Sums(RepeatN(1, 5))))
	})
}

func TestSwap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Swap(Empty2[int, string])))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t,
			pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}},
			collect2(Swap(pairs[string, int]{{"a", 1}, {"b", 2}, {"c", 3}}.All)))
	})
}

func TestTake(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Take(Empty[int], 4)))
	})

	t.Run("non-empty seq, take none", func(t *testing.T) {
		assert.Empty(t, slices.Collect(Take(Count(1, 1), 0)))
	})

	t.Run("non-empty seq, take some", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3}, slices.Collect(Take(Count(1, 1), 3)))
	})

	t.Run("should force only necessary items", func(t *testing.T) {
		assert.NotPanics(t, func() {
			assert.Equal(t, []int{1, 2}, slices.Collect(Take(func(yield func(int) bool) {
				if !yield(1) {
					return
				}
				if !yield(2) {
					return
				}
				panic("should not be evaluated")
			}, 2)))
		})
	})
}

func TestTake2(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(Take2(Empty2[int, string], 4)))
	})

	t.Run("non-empty seq, take none", func(t *testing.T) {
		assert.Empty(t, collect2(Take2(Enumerate(Count("", "a")), 0)))
	})

	t.Run("non-empty seq, take some", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{0, ""}, {1, "a"}, {2, "aa"}},
			collect2(Take2(Enumerate(Count("", "a")), 3)))
	})

	t.Run("should force only necessary pairs", func(t *testing.T) {
		assert.NotPanics(t, func() {
			assert.Equal(t, pairs[int, string]{{0, ""}, {1, "a"}},
				collect2(Take2(func(yield func(int, string) bool) {
					if !yield(0, "") {
						return
					}
					if !yield(1, "a") {
						return
					}
					panic("should not be evaluated")
				}, 2)))
		})
	})
}

func TestTakeWhile(t *testing.T) {
	t.Run("should return empty seq for empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(TakeWhile(Empty[int], func(int) bool { return true })))
	})
	t.Run("should return empty seq for const false pred", func(t *testing.T) {
		assert.Empty(t, slices.Collect(TakeWhile(Repeat(0), func(int) bool { return false })))
	})
	t.Run("should return a prefix of non-empty seq matching pred", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3}, slices.Collect(TakeWhile(FromValues(1, 2, 3, 4, 3, 2, 1), func(v int) bool { return v < 4 })))
	})
	t.Run("should short-circuit", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3}, slices.Collect(TakeWhile(Count(1, 1), func(v int) bool { return v < 4 })))
	})
}

func TestTakeWhile2(t *testing.T) {
	t.Run("should return empty seq for empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(TakeWhile2(Empty2[int, string], func(int, string) bool { return true })))
	})
	t.Run("should return empty seq for const false pred", func(t *testing.T) {
		assert.Empty(t, collect2(TakeWhile2(Repeat2(0, ""), func(int, string) bool { return false })))
	})
	t.Run("should return a prefix of non-empty seq matching pred", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{1, "a"}, {2, "aa"}},
			collect2(TakeWhile2(pairs[int, string]{{1, "a"}, {2, "aa"}, {3, "bbb"}, {4, "aaaa"}, {3, "aaa"}, {2, "aa"}}.All,
				func(v int, s string) bool { return v < 4 && strings.Contains(s, "a") })))
	})
	t.Run("should short-circuit", func(t *testing.T) {
		assert.Equal(t, pairs[int, string]{{0, ""}, {1, ""}, {2, ""}}, collect2(TakeWhile2(Enumerate(Repeat("")), func(v int, _ string) bool { return v < 3 })))
	})
}

func TestUnfold2(t *testing.T) {
	t.Run("should yield all generated pairs", func(t *testing.T) {
		seq := Unfold2('a', func(stt byte) (string, int, bool, byte) {
			if stt > 'c' {
				return "", 0, false, stt
			}
			return string(stt), int(stt), true, stt + 1
		})
		assert.Equal(t, pairs[string, int]{{"a", 'a'}, {"b", 'b'}, {"c", 'c'}}, collect2(seq))
	})
	t.Run("break early", func(t *testing.T) {
		seq := Unfold2("a", func(stt string) (string, int, bool, string) {
			return stt, len(stt), true, stt + stt
		})
		assert.Equal(t, pairs[string, int]{{"a", 1}, {"aa", 2}, {"aaaa", 4}}, collect2(Take2(seq, 3)))
	})
}

func TestUnpackMap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		assert.Empty(t, collect2(UnpackMap(Empty[int], func(v int) (float64, byte) { return float64(v), byte(v) })))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		assert.Equal(t, pairs[float64, byte]{{1, 1}, {2, 2}, {3, 3}},
			collect2(UnpackMap(FromValues(1, 2, 3), func(v int) (float64, byte) { return float64(v), byte(v) })))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, pairs[float64, byte]{{1, 1}, {2, 2}},
			collect2(Take2(UnpackMap(FromValues(1, 2, 3), func(v int) (float64, byte) { return float64(v), byte(v) }), 2)))
	})
}

func TestUnzip(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		seq1, seq2 := Unzip(Empty2[int, string])
		assert.Empty(t, slices.Collect(seq1))
		assert.Empty(t, slices.Collect(seq2))
	})

	t.Run("non-empty seq", func(t *testing.T) {
		seq1, seq2 := Unzip(pairs[int, string]{{1, "a"}, {2, "b"}, {3, "c"}}.All)
		assert.Equal(t, []int{1, 2, 3}, slices.Collect(seq1))
		assert.Equal(t, []string{"a", "b", "c"}, slices.Collect(seq2))
	})
}

func TestZipMany(t *testing.T) {
	t.Run("no seqs", func(t *testing.T) {
		assert.Empty(t, slices.Collect(ZipMany[int]()))
	})

	t.Run("empty seqs", func(t *testing.T) {
		assert.Empty(t, slices.Collect(ZipMany(Empty[int], Empty[int])))
	})

	t.Run("one empty seq", func(t *testing.T) {
		assert.Empty(t, slices.Collect(ZipMany(FromValues(1, 2, 3), Empty[int], FromValues(3, 4, 5))))
	})

	t.Run("mixed length seqs", func(t *testing.T) {
		assert.Equal(t, [][]int{{1, 2, 3}, {2, 3, 4}},
			slices.Collect(ZipMany(FromValues(1, 2, 3), FromValues(2, 3), FromValues(3, 4, 5, 6))))
	})

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, [][]int{{1, 2, 3}, {2, 3, 4}, {3, 4, 5}},
			slices.Collect(Take(ZipMany(Count(1, 1), Count(2, 1), Count(3, 1)), 3)))
	})
}

func id[T any](t T) T {
	return t
}

func collect2[Fst, Snd any](seq Seq2[Fst, Snd]) (pairs pairs[Fst, Snd]) {
	for fst, snd := range seq {
		pairs = append(pairs, pair[Fst, Snd]{
			Fst: fst,
			Snd: snd,
		})
	}
	return
}
