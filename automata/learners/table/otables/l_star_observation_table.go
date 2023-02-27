package otables

import (
	"github.com/neuralchecker/go-adt/dictionary"
	"github.com/neuralchecker/go-adt/set"
	"github.com/neuralchecker/go-automata/interfaces"
)

type LStarTable[T any] struct {
	*observationTable[T]
}

func NewLStarTable[T any]() *LStarTable[T] {
	return &LStarTable[T]{
		&observationTable[T]{
			red:          set.NewUnordered[interfaces.Sequence[T]](),
			blue:         set.NewUnordered[interfaces.Sequence[T]](),
			observations: dictionary.NewUnordered[interfaces.Sequence[T], []bool](),
		},
	}
}

func (l *LStarTable[T]) IsClosed() bool {
	it := l.blue.Iterator()
	for it.HasNext() {
		s := it.Next()
		if !l.SameRowExistsInRed(s) {
			return false
		}
	}
	return true
}

func (l *LStarTable[T]) SameRowExistsInRed(blueS interfaces.Sequence[T]) bool {
	it := l.red.Iterator()
	for it.HasNext() {
		redS := it.Next()
		blueObs, _ := l.observations.Get(blueS)
		redObs, _ := l.observations.Get(redS)
		if eqArr(blueObs, redObs) {
			return true
		}
	}
	return false
}

func (l *LStarTable[T]) FindInconsistency(a interfaces.Alphabet[T]) (Inconsistency[T], bool) {
	redSlice := l.red.ToSlice()
	for i := range redSlice {
		for j := i + 1; j < len(redSlice); j++ {
			red1 := redSlice[i]
			red2 := redSlice[j]
			if red1.Equal(red2) {
				continue
			}
			obs1, _ := l.observations.Get(red1)
			obs2, _ := l.observations.Get(red2)
			if !eqArr(obs1, obs2) {
				continue
			}
			if inconsistency, ok := l.inconsistencyBetween(red1, red2, a); ok {
				return inconsistency, true
			}
		}
	}
	return Inconsistency[T]{}, false
}
