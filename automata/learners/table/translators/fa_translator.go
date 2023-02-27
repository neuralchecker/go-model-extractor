package translators

import (
	"fmt"
	"time"

	"github.com/neuralchecker/go-adt/dictionary"
	"github.com/neuralchecker/go-adt/set"
	"github.com/neuralchecker/go-automata/automata"
	"github.com/neuralchecker/go-automata/base_types/sequences"
	"github.com/neuralchecker/go-automata/base_types/states"
	"github.com/neuralchecker/go-automata/interfaces"
)

type FaTranslator[T any] struct{}

type faTObservationTable[T any] interface {
	GetObservation(suffSeq interfaces.Sequence[T]) []bool
	GetRed() set.Set[interfaces.Sequence[T]]
}

func (t FaTranslator[T]) Translate(name string, oTable faTObservationTable[T], a interfaces.Alphabet[T]) (automata.FiniteAutomaton[T], error) {
	epsilon := sequences.New[T]()

	sequenceStates, obsStates := t.getStatesFromOTable(oTable, a)
	it := sequenceStates.Iterator()
	for it.HasNext() {
		sequence, state := it.Next().Unwrap()
		for _, symbol := range a.GetSymbols() {
			suffSeq := sequence.Append(symbol)
			obs := oTable.GetObservation(suffSeq)
			if ok, transitionState := t.findStateWithObservation(obs, obsStates); ok {
				state.AddTransition(symbol, transitionState)
			}
		}
	}

	epsilonState, _ := sequenceStates.Get(epsilon)
	return &automata.DeterministicFiniteAutomaton[T]{
		Name:         name,
		Alphabet:     a,
		InitialState: epsilonState,
		States:       sequenceStates.Values(),
	}, nil
}

func (t FaTranslator[T]) TransaleWithDefualtName(a interfaces.Alphabet[T], oTable faTObservationTable[T]) (automata.FiniteAutomaton[T], error) {
	return t.Translate(fmt.Sprintf("dfa-%d", time.Now().Unix()), oTable, a)
}

func (t FaTranslator[T]) getStatesFromOTable(oTable faTObservationTable[T], a interfaces.Alphabet[T]) (
	dictionary.Dictionary[interfaces.Sequence[T], *states.FAState[T]], map[string]*states.FAState[T]) {
	sequenceStates := dictionary.NewUnorderedSize[interfaces.Sequence[T], *states.FAState[T]](oTable.GetRed().Size())
	obsStates := make(map[string]*states.FAState[T], oTable.GetRed().Size())
	addedRows := make(map[string]struct{})
	it := oTable.GetRed().Iterator()
	for it.HasNext() {
		sequence := it.Next()
		obs := obs2String(oTable.GetObservation(sequence))
		if _, ok := addedRows[obs]; !ok {
			state := states.NewFAState[T](sequence.String(), obs[0] == '1')
			sequenceStates.Set(sequence, state)
			obsStates[obs] = state
			addedRows[obs] = struct{}{}
		}
	}

	return sequenceStates, obsStates
}

func (t *FaTranslator[T]) findStateWithObservation(obs []bool, obsStates map[string]*states.FAState[T]) (bool, *states.FAState[T]) {
	obsString := obs2String(obs)
	state, ok := obsStates[obsString]
	return ok, state
}

func obs2String(bs []bool) string {
	s := make([]byte, 0, len(bs))
	for _, b := range bs {
		if b {
			s = append(s, '1')
		} else {
			s = append(s, '0')
		}
	}
	return string(s)
}
