package seqs

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_pairUnwrap(t *testing.T) {
	fst, snd := pairOf(42, "42").Unwrap()
	assert.Equal(t, 42, fst)
	assert.Equal(t, "42", snd)
}
