package seqs

type Pair[First, Second any] interface {
	Unwrap() (First, Second)
}

type pair[First, Second any] struct {
	First  First
	Second Second
}

func (p pair[First, Second]) Unwrap() (First, Second) {
	return p.First, p.Second
}

func pairOf[First, Second any](first First, second Second) Pair[First, Second] {
	return pair[First, Second]{
		First:  first,
		Second: second,
	}
}

func first[P Pair[First, Second], First, Second any](pair P) First {
	first, _ := pair.Unwrap()
	return first
}

func second[P Pair[First, Second], First, Second any](pair P) Second {
	_, second := pair.Unwrap()
	return second
}
