package teachers

import (
	"github.com/neuralchecker/go-automata/automata"
	"github.com/neuralchecker/go-automata/comparators"
	"github.com/neuralchecker/go-automata/interfaces"
	"github.com/neuralchecker/go-model-extractor/automata/learners"
)

type automatonTeacher[T any] struct {
	automaton          automata.Automaton[T]
	comparisonStrategy automata.Comparator[T]
	mqCount            int
	eqCount            int
}

var _ learners.Teacher[struct{}] = (*automatonTeacher[struct{}])(nil)

func NewAutomatonTeacher[T any](a automata.Automaton[T], cs automata.Comparator[T]) *automatonTeacher[T] {
	return &automatonTeacher[T]{
		automaton:          a,
		comparisonStrategy: cs,
	}
}

func (t *automatonTeacher[T]) GetAlphabet() interfaces.Alphabet[T] {
	return t.automaton.GetAlphabet()
}

func (t *automatonTeacher[T]) MembershipQueryCount() int {
	return t.mqCount
}

func (t *automatonTeacher[T]) EquivalenceQueryCount() int {
	return t.eqCount
}

func (t *automatonTeacher[T]) ResetStatistics() {
	t.mqCount = 0
	t.eqCount = 0
}

func (t *automatonTeacher[T]) MembershipQuery(s interfaces.Sequence[T]) (bool, error) {
	t.mqCount++
	ok, err := t.automaton.Accepts(s)
	if err != nil {
		return false, err
	}
	return ok, nil
}

func (t *automatonTeacher[T]) EquivalenceQuery(a automata.FiniteAutomaton[T]) (interfaces.Sequence[T], bool, error) {
	t.eqCount++
	cEx, err := t.comparisonStrategy.GetCounterexampleBetween(a, t.automaton)
	if err != nil {
		if err == comparators.ErrNoCounterexampleFound {
			return nil, true, nil
		} else {
			return nil, false, err
		}
	}

	return cEx, false, nil
}
