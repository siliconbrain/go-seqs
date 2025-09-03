package mapseqs

import (
	"github.com/siliconbrain/go-seqs/seqs"
)

type Mapping[K comparable, V any] interface {
	~map[K]V
}

func EntriesOf[
	Map Mapping[Key, Value],
	Key comparable,
	Value any,
](
	m Map,
) MapEntries[
	Map,
	Key,
	Value,
	MapEntry[Key, Value],
	func(Key, Value) MapEntry[Key, Value],
] {
	return EntriesOfWith(m, MapEntryFrom)
}

func EntriesOfWith[
	Map Mapping[Key, Value],
	Key comparable,
	Value any,
	Entry any,
	Pack ~func(Key, Value) Entry,
](m Map, pack Pack) MapEntries[Map, Key, Value, Entry, Pack] {
	return MapEntries[Map, Key, Value, Entry, Pack]{
		Map:  m,
		Pack: pack,
	}
}

func KeysOf[M Mapping[K, V], K comparable, V any](m M) MapEntries[M, K, V, K, func(K, V) K] {
	return EntriesOfWith(m, func(k K, _ V) K { return k })
}

func ValuesOf[M Mapping[K, V], K comparable, V any](m M) MapEntries[M, K, V, V, func(K, V) V] {
	return EntriesOfWith(m, func(_ K, v V) V { return v })
}

type MapEntries[
	Map Mapping[Key, Value],
	Key comparable,
	Value any,
	Entry any,
	Pack ~func(Key, Value) Entry,
] struct {
	Map  Map
	Pack Pack
}

func (mes MapEntries[_, K, V, E, _]) ForEachUntil(yield func(E) bool) {
	for k, v := range mes.Map {
		if yield(mes.Pack(k, v)) {
			return
		}
	}
}

func (mes MapEntries[_, _, _, _, _]) Len() int {
	return len(mes.Map)
}

var _ seqs.Seq[any] = (*MapEntries[map[string]any, string, any, any, func(string, any) any])(nil)
var _ seqs.Lener = (*MapEntries[map[string]any, string, any, any, func(string, any) any])(nil)

func MapEntryFrom[Key, Value any](key Key, value Value) MapEntry[Key, Value] {
	return MapEntry[Key, Value]{
		Key:   key,
		Value: value,
	}
}

type MapEntry[Key, Value any] struct {
	Key   Key
	Value Value
}

func (e MapEntry[Key, Value]) Unpack() (Key, Value) {
	return e.Key, e.Value
}

func ToMap[
	Seq seqs.Seq[Entry],
	Entry interface{ Unpack() (Key, Value) },
	Key comparable,
	Value any,
](seq Seq) map[Key]Value {
	return ToMapWith(seq, Entry.Unpack)
}

func ToMapWith[Seq seqs.Seq[Entry], Entry any, Key comparable, Value any, Unpack ~func(Entry) (Key, Value)](seq Seq, unpack Unpack) (m map[Key]Value) {
	if lener, ok := any(seq).(seqs.Lener); ok {
		m = make(map[Key]Value, lener.Len())
	} else {
		m = make(map[Key]Value)
	}
	return seqs.SeededReduce(seq, m, func(m map[Key]Value, entry Entry) map[Key]Value {
		key, value := unpack(entry)
		m[key] = value
		return m
	})
}

// ZipToMap returns a map obtained by zipping the specified sequence of keys and sequence of values.
func ZipToMap[KeySeq seqs.Seq[Key], ValueSeq seqs.Seq[Value], Key comparable, Value any](keys KeySeq, values ValueSeq) map[Key]Value {
	return ToMap(seqs.ZipWith(keys, values, MapEntryFrom))
}
