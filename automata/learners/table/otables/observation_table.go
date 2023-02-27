package otables

import (
	"fmt"
	"strings"

	"github.com/neuralchecker/go-adt/dictionary"
	"github.com/neuralchecker/go-adt/set"
	"github.com/neuralchecker/go-automata/interfaces"
)

type observationTable[T any] struct {
	red          set.Set[interfaces.Sequence[T]]
	blue         set.Set[interfaces.Sequence[T]]
	observations dictionary.Dictionary[interfaces.Sequence[T], []bool]
	exp          []interfaces.Sequence[T]
}

func (ot *observationTable[T]) GetObservations() dictionary.Dictionary[interfaces.Sequence[T], []bool] {
	return ot.observations
}

func (ot *observationTable[T]) GetObservation(s interfaces.Sequence[T]) []bool {
	obs, _ := ot.observations.Get(s)
	return obs
}

func (ot *observationTable[T]) AppendObservations(s interfaces.Sequence[T], obs ...bool) {
	obsOld, _ := ot.observations.Get(s)
	obsOld = append(obsOld, obs...)
	ot.observations.Set(s, obsOld)
}

func (ot *observationTable[T]) MoveFromBlueToRed(s interfaces.Sequence[T]) {
	if ok := ot.blue.Contains(s); ok {
		ot.blue.Remove(s)
		ot.red.Add(s)
	}
}

func (ot *observationTable[T]) GetRed() set.Set[interfaces.Sequence[T]] {
	return ot.red
}

func (ot *observationTable[T]) AddToRed(s interfaces.Sequence[T]) {
	ot.red.Add(s)
}

func (ot *observationTable[T]) IsInRed(s interfaces.Sequence[T]) bool {
	return ot.red.Contains(s)
}

func (ot *observationTable[T]) GetBlue() set.Set[interfaces.Sequence[T]] {
	return ot.blue
}

func (ot *observationTable[T]) AddToBlue(s interfaces.Sequence[T]) {
	ot.blue.Add(s)
}

func (ot *observationTable[T]) IsInBlue(s interfaces.Sequence[T]) bool {
	return ot.blue.Contains(s)
}

func (ot *observationTable[T]) GetExp() []interfaces.Sequence[T] {
	return ot.exp
}

func (ot *observationTable[T]) AddExp(s interfaces.Sequence[T]) {
	ot.exp = append(ot.exp, s)
}

func (ot *observationTable[T]) inconsistencyBetween(s1, s2 interfaces.Sequence[T],
	alphabet interfaces.Alphabet[T]) (Inconsistency[T], bool) {
	for _, symbol := range alphabet.GetSymbols() {
		suffSeq1 := s1.Append(symbol)
		suffSeq2 := s2.Append(symbol)
		diffSeq, hasDiff := ot.observationDifferenceBetween(suffSeq1, suffSeq2)
		if hasDiff {
			return Inconsistency[T]{
				Sequence1:          s1,
				Sequence2:          s2,
				Symbol:             symbol,
				DifferenceSequence: diffSeq,
			}, true
		}
	}
	return Inconsistency[T]{}, false
}

func (ot *observationTable[T]) observationDifferenceBetween(s1, s2 interfaces.Sequence[T]) (interfaces.Sequence[T], bool) {
	obs1, _ := ot.observations.Get(s1)
	obs2, _ := ot.observations.Get(s2)
	if len(obs1) != len(obs2) {
		panic(
			fmt.Errorf("observation sequences must never have different lengths, but got %d and %d",
				len(obs1), len(obs2)),
		)
	}

	for i := range obs1 {
		if obs1[i] != obs2[i] {
			return ot.exp[i], true
		}
	}
	return nil, false
}

func (ot *observationTable[T]) String() string {
	strBuilder := &strings.Builder{}
	strBuilder.WriteString("Observation Table:\n")
	strBuilder.WriteString("Red:\n")
	strBuilder.WriteString(fmt.Sprintf("%v\n", ot.red))
	strBuilder.WriteString("Blue:\n")
	strBuilder.WriteString(fmt.Sprintf("%v\n", ot.blue))
	strBuilder.WriteString("Observations:\n")
	it := ot.observations.Iterator()
	for it.HasNext() {
		k, v := it.Next().Unwrap()
		strBuilder.WriteString(fmt.Sprintf("%v -> %v\n", k, v))
	}
	strBuilder.WriteString("Exp:\n")
	strBuilder.WriteString(fmt.Sprintf("%v\n", ot.exp))
	return strBuilder.String()
}
