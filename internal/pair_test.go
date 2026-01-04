package internal

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_Pair(t *testing.T) {
	fst, snd := PairFrom(42, "42").Unpack()
	assert.Equal(t, 42, fst)
	assert.Equal(t, "42", snd)
}
