package learners

import (
	"context"

	"github.com/neuralchecker/go-automata/automata"
	"github.com/neuralchecker/go-automata/interfaces"
)

type Learner[T any] interface {
	Learn(Teacher[T]) (LearningResult[T], error)
}

type LearnerCtx[T any] interface {
	LearnCtx(ctx context.Context, teacher Teacher[T]) (LearningResult[T], error)
}

type FullLearner[T any] interface {
	Learner[T]
	LearnerCtx[T]
}

type Teacher[T any] interface {
	// GetAlphabet returns the alphabet of the teacher, usually the same as the target model.
	GetAlphabet() interfaces.Alphabet[T]
	// MembershipQueryCount returns the number of membership queries performed by the teacher.
	MembershipQueryCount() int
	// EquivalenceQueryCount returns the number of equivalence queries performed by the teacher.
	EquivalenceQueryCount() int
	// MembershipQuery returns true if the given sequence is a member of the target model.
	MembershipQuery(interfaces.Sequence[T]) (bool, error)
	// EquivalenceQuery returns true if the given automaton is equivalent to the target model.
	EquivalenceQuery(automata.FiniteAutomaton[T]) (interfaces.Sequence[T], bool, error)
	// ResetStatistics resets the membership and equivalence query counters.
	ResetStatistics()
}

type LearningResult[T any] struct {
	// Model is the learned model.
	Model automata.FiniteAutomaton[T]
	// StateCount is the number of states in the learned model.
	StateCount int
	// Info is a map of additional information about the learning process.
	Info map[string]any
}
