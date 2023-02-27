package table_test

import (
	"math/rand"
	"testing"

	"github.com/neuralchecker/go-automata/automata/definitions"
	"github.com/neuralchecker/go-automata/comparators"
	"github.com/neuralchecker/go-model-extractor/automata/learners/table"
	"github.com/neuralchecker/go-model-extractor/teachers"
	"github.com/stretchr/testify/assert"
)

var (
	learner *table.LStarLearner[rune]
)

func TestMain(m *testing.M) {
	learner = table.NewLStarLearner[rune]()

	m.Run()
}

func TestTomitas1(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas1()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}

func TestTomitas2(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas2()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}

func TestTomitas3(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas3()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}

func TestTomitas4(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas4()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}

func TestTomitas5(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas5()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}

func TestTomitas6(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas6()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}

func TestTomitas7(t *testing.T) {
	rand := rand.New(rand.NewSource(42))
	comparator := comparators.NewRandomWalk[rune](10000, float64(0.1), rand)
	grammar1 := definitions.GetTomitas7()
	teacher := teachers.NewAutomatonTeacher[rune](grammar1, comparator)

	result, err := learner.Learn(teacher)
	assert.NoError(t, err)
	equal, err := comparator.AreEquivalent(result.Model, grammar1)
	assert.NoError(t, err)
	assert.True(t, equal)
}
