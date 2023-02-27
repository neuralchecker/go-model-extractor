package otables

import (
	"github.com/neuralchecker/go-automata/interfaces"
)

type Inconsistency[T any] struct {
	Sequence1          interfaces.Sequence[T]
	Sequence2          interfaces.Sequence[T]
	Symbol             interfaces.Symbol[T]
	DifferenceSequence interfaces.Sequence[T]
}
