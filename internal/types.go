package internal

// Pred is a predicate over a value.
type Pred[Value any] = func(Value) bool

// Pred2 is a predicate over a pair of values.
type Pred2[Value1, Value2 any] = func(Value1, Value2) bool

// Summable lists types that support addition using the + operator.
type Summable interface {
	~complex64 | ~complex128 |
		~float32 | ~float64 |
		~int | ~int8 | ~int16 | ~int32 /* rune */ | ~int64 |
		~uint | ~uint8 /* byte */ | ~uint16 | ~uint32 | ~uint64 | ~uintptr |
		string
}
