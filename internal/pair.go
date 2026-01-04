package internal

func PairFrom[Value1, Value2 any](value1 Value1, value2 Value2) Pair[Value1, Value2] {
	return Pair[Value1, Value2]{
		Value1: value1,
		Value2: value2,
	}
}

type Pair[Value1, Value2 any] struct {
	Value1 Value1
	Value2 Value2
}

func (p Pair[Value1, Value2]) Unpack() (Value1, Value2) {
	return p.Value1, p.Value2
}
