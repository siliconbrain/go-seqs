package iter

import "github.com/siliconbrain/go-seqs/internal"

type pair[Value1, Value2 any] = internal.Pair[Value1, Value2]

type pairs[Value1, Value2 any] []pair[Value1, Value2]

func (ps pairs[Value1, Value2]) All(yield func(Value1, Value2) bool) {
	for _, p := range ps {
		if !yield(p.Unpack()) {
			return
		}
	}
}
