package otables

import "github.com/google/go-cmp/cmp"

func eqArr[T any](a, b []T) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !cmp.Equal(a[i], b[i]) {
			return false
		}
	}
	return true
}
