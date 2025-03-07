package mapseqs

import (
	"testing"

	"github.com/siliconbrain/go-seqs/seqs"
	"github.com/stretchr/testify/require"
)

func TestEntriesOf(t *testing.T) {
	t.Run("nil map", func(t *testing.T) {
		require.Empty(t, seqs.ToSlice(EntriesOf(map[string]any(nil))))
	})
	t.Run("empty map", func(t *testing.T) {
		require.Empty(t, seqs.ToSlice(EntriesOf(map[string]any{})))
	})
	t.Run("non-empty map", func(t *testing.T) {
		require.ElementsMatch(t,
			[]MapEntry[string, any]{
				{"foo", 42},
				{"bar", false},
			},
			seqs.ToSlice(EntriesOf(map[string]any{
				"foo": 42,
				"bar": false,
			})))
	})
}

func TestKeysOf(t *testing.T) {
	t.Run("nil map", func(t *testing.T) {
		require.Empty(t, seqs.ToSlice(KeysOf(map[string]any(nil))))
	})
	t.Run("empty map", func(t *testing.T) {
		require.Empty(t, seqs.ToSlice(KeysOf(map[string]any{})))
	})
	t.Run("non-empty map", func(t *testing.T) {
		require.ElementsMatch(t, []string{"foo", "bar"}, seqs.ToSlice(KeysOf(map[string]any{"foo": 42, "bar": false})))
	})
}

func TestValuesOf(t *testing.T) {
	t.Run("nil map", func(t *testing.T) {
		require.Empty(t, seqs.ToSlice(ValuesOf(map[string]any(nil))))
	})
	t.Run("empty map", func(t *testing.T) {
		require.Empty(t, seqs.ToSlice(ValuesOf(map[string]any{})))
	})
	t.Run("non-empty map", func(t *testing.T) {
		require.ElementsMatch(t, []any{42, false}, seqs.ToSlice(ValuesOf(map[string]any{"foo": 42, "bar": false})))
	})
}

func TestRoundTrip(t *testing.T) {
	m := map[string]any{
		"foo": 42,
		"bar": false,
	}
	require.Equal(t, m, ToMap(EntriesOf(m)))
}
