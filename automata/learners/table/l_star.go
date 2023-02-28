package table

import (
	"context"

	"github.com/neuralchecker/go-adt/set"
	"github.com/neuralchecker/go-automata/automata"
	"github.com/neuralchecker/go-automata/base_types/sequences"
	"github.com/neuralchecker/go-automata/interfaces"
	"github.com/neuralchecker/go-model-extractor/automata/learners"
	"github.com/neuralchecker/go-model-extractor/automata/learners/table/otables"
	"github.com/neuralchecker/go-model-extractor/automata/learners/table/translators"
)

// LStarLearner's zero value is a valid L* learner.
type LStarLearner[T any] struct {
	// The name for the learned automaton. If empty, the name is set to "L*".
	Name string
	// Pre is a list of functions to be called before the learning process starts.
	// If ok is true, the result of the function is added to info.
	Pre []func() (key string, value any, ok bool)
	// Post is a list of functions to be called after the learning process ends.
	// If ok is true, the result of the function is added to info.
	Post            []func() (key string, value any, ok bool)
	modelTranslator translators.FaTranslator[T]
}

// This is just a way to make sure that LStarLearner implements learners.FullLearner[T].
var _ learners.FullLearner[int] = &LStarLearner[int]{}

// NewLStarLearner is the same as using a pointer to the zero value of LStarLearner[T].
func NewLStarLearner[T any]() *LStarLearner[T] {
	return &LStarLearner[T]{}
}

func (l *LStarLearner[T]) AddPre(f func() (key string, value any, ok bool)) {
	l.Pre = append(l.Pre, f)
}

func (l *LStarLearner[T]) AddPost(f func() (key string, value any, ok bool)) {
	l.Post = append(l.Post, f)
}

func (l *LStarLearner[T]) SetName(name string) {
	l.Name = name
}

func (l *LStarLearner[T]) GetName() string {
	if l.Name == "" {
		return "L*"
	}
	return l.Name
}

func (l *LStarLearner[T]) LearnCtx(ctx context.Context, teacher learners.Teacher[T]) (learners.LearningResult[T], error) {
	type learnResult struct {
		oTable *otables.LStarTable[T]
		model  automata.FiniteAutomaton[T]
		err    error
	}
	result := learners.LearningResult[T]{Info: make(map[string]any)}
	output := make(chan learnResult)
	go func() {
		oTable, model, err := l.learn(teacher)
		output <- learnResult{oTable, model, err}
	}()

	// PRE
	l.runPre(&result)

	// Loop until we get a result or the context is done.
	for {
		select {
		// If the context is done, return the result and the error.
		case <-ctx.Done():
			return result, ctx.Err()

		// If we get a result, return it.
		case res := <-output:
			// If there was an error, return it.
			if res.err != nil {
				return result, res.err
			}
			// Otherwise, load the result.

			l.loadResult(&result, res.model, teacher, res.oTable)
			// POST
			l.runPost(&result)
			return result, nil
		}
	}
}

// Learn implements learners.Learner
func (l *LStarLearner[T]) Learn(teacher learners.Teacher[T]) (learners.LearningResult[T], error) {
	var (
		result = learners.LearningResult[T]{Info: make(map[string]any)}
	)
	// PRE
	l.runPre(&result)

	// LEARN
	oTable, model, err := l.learn(teacher)
	if err != nil {
		return result, err
	}

	l.loadResult(&result, model, teacher, oTable)
	// POST
	l.runPost(&result)

	return result, nil
}

func (l *LStarLearner[T]) learn(teacher learners.Teacher[T]) (*otables.LStarTable[T], automata.FiniteAutomaton[T], error) {
	var (
		equals = false
		model  automata.FiniteAutomaton[T]
	)
	name := l.GetName()
	oTable := otables.NewLStarTable[T]()
	l.initializeOTable(oTable, teacher)
	for !equals {
		var (
			err            error
			counterExample interfaces.Sequence[T]
		)
		l.closeOTable(oTable, teacher)
		l.makeOTableConsistent(oTable, teacher)
		model, err = l.modelTranslator.Translate(name, oTable, teacher.GetAlphabet())
		if err != nil {
			return oTable, model, err
		}
		counterExample, equals, err = teacher.EquivalenceQuery(model)
		if err != nil {
			return oTable, model, err
		}
		if !equals {
			l.updateOTableWithCounterExample(oTable, counterExample, teacher)
		}
	}
	return oTable, model, nil
}

func (l *LStarLearner[T]) closeOTable(oTable *otables.LStarTable[T], teacher learners.Teacher[T]) {
	for {
		blueSeq, ok := l.getViolationSequence(oTable)
		if !ok {
			return
		}
		oTable.MoveFromBlueToRed(blueSeq)
		for _, symbol := range teacher.GetAlphabet().GetSymbols() {
			newBlue := blueSeq.Append(symbol)
			l.addToBlue(newBlue, oTable, teacher)
		}

	}
}

func (l *LStarLearner[T]) getViolationSequence(oTable *otables.LStarTable[T]) (interfaces.Sequence[T], bool) {
	it := oTable.GetBlue().Iterator()
	for it.HasNext() {
		blueSeq := it.Next()
		if !oTable.SameRowExistsInRed(blueSeq) {
			return blueSeq, true
		}
	}
	return nil, false
}

func (l *LStarLearner[T]) makeOTableConsistent(oTable *otables.LStarTable[T], teacher learners.Teacher[T]) {
	for {
		inconsistency, ok := oTable.FindInconsistency(teacher.GetAlphabet())
		if !ok {
			return
		}
		l.resolveInconsistency(inconsistency, oTable, teacher)
		l.closeOTable(oTable, teacher)
	}
}

func (l *LStarLearner[T]) resolveInconsistency(inconsistency otables.Inconsistency[T],
	oTable *otables.LStarTable[T], teacher learners.Teacher[T]) error {

	newExp := sequences.New(inconsistency.Symbol).
		Append(inconsistency.DifferenceSequence.AsSlice()...)
	oTable.AddExp(newExp)
	for _, seq := range oTable.GetObservations().Keys() {
		obs, err := teacher.MembershipQuery(seq.Append(newExp.AsSlice()...))
		if err != nil {
			return err
		}
		oTable.AppendObservations(seq, obs)
	}
	return nil
}

func (l *LStarLearner[T]) updateOTableWithCounterExample(oTable *otables.LStarTable[T],
	counterExample interfaces.Sequence[T], teacher learners.Teacher[T]) {
	prefixes := counterExample.GetPreffixes()
	prefSet := l.setFromSlice(prefixes)
	for _, prefix := range prefixes {
		oTable.AddToRed(prefix)
		for _, symbol := range teacher.GetAlphabet().GetSymbols() {
			newBlue := prefix.Append(symbol)
			if ok := prefSet.Contains(newBlue); !ok {
				l.addToBlue(newBlue, oTable, teacher)
			}
		}
	}
}

func (l *LStarLearner[T]) setFromSlice(seq []interfaces.Sequence[T]) set.Set[interfaces.Sequence[T]] {
	set := set.NewUnorderedSize[interfaces.Sequence[T]](len(seq))
	for _, s := range seq {
		set.Add(s)
	}
	return set
}

func (l *LStarLearner[T]) runPre(result *learners.LearningResult[T]) {
	if l.Pre == nil {
		return
	}
	for _, f := range l.Post {
		if k, v, ok := f(); ok {
			result.Info[k] = v
		}
	}
}

func (l *LStarLearner[T]) runPost(result *learners.LearningResult[T]) {
	if l.Post == nil {
		return
	}
	for _, f := range l.Post {
		if k, v, ok := f(); ok {
			result.Info[k] = v
		}
	}
}

func (l *LStarLearner[T]) loadResult(result *learners.LearningResult[T],
	model automata.FiniteAutomaton[T], teacher learners.Teacher[T], oTable *otables.LStarTable[T]) {

	result.Model = model
	result.StateCount = len(model.GetStates())
	result.Info["equivalence_queries_count"] = teacher.EquivalenceQueryCount()
	result.Info["membership_queries_count"] = teacher.MembershipQueryCount()
	result.Info["table"] = oTable
}

func (l *LStarLearner[T]) initializeOTable(oTable *otables.LStarTable[T], teacher learners.Teacher[T]) error {

	epsilon := sequences.New[T]()
	oTable.AddExp(epsilon)
	if err := l.addToRed(epsilon, oTable, teacher); err != nil {
		return err
	}
	for _, symbol := range teacher.GetAlphabet().GetSymbols() {
		if err := l.addToBlue(sequences.New(symbol), oTable, teacher); err != nil {
			return err
		}
	}
	return nil
}

func (l *LStarLearner[T]) addToRed(seq interfaces.Sequence[T],
	oTable *otables.LStarTable[T], teacher learners.Teacher[T]) error {

	if !oTable.IsInRed(seq) {
		oTable.AddToRed(seq)
		observations, err := l.getObservation(seq, oTable, teacher)
		if err != nil {
			return err
		}
		oTable.AppendObservations(seq, observations...)
	}
	return nil
}

func (l *LStarLearner[T]) addToBlue(seq interfaces.Sequence[T],
	oTable *otables.LStarTable[T], teacher learners.Teacher[T]) error {

	if !oTable.IsInBlue(seq) {
		oTable.AddToBlue(seq)
		observations, err := l.getObservation(seq, oTable, teacher)
		if err != nil {
			return err
		}
		oTable.AppendObservations(seq, observations...)
	}

	return nil
}

func (l *LStarLearner[T]) getObservation(seq interfaces.Sequence[T],
	oTable *otables.LStarTable[T], teacher learners.Teacher[T]) ([]bool, error) {

	requiredSuffixes := oTable.GetExp()
	row := make([]bool, 0, len(requiredSuffixes))
	for _, suffix := range requiredSuffixes {
		obs, err := teacher.MembershipQuery(seq.Append(suffix.AsSlice()...))
		if err != nil {
			return nil, err
		}
		row = append(row, obs)
	}

	return row, nil
}
