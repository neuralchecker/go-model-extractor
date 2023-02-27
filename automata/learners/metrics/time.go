package metrics

import "time"

type Time struct {
	start time.Time
}

type prePost interface {
	Pre(func() (string, any, bool))
	Post(func() (string, any, bool))
}

func (t *Time) Register(p prePost) {
	p.Pre(func() (string, any, bool) {
		t.start = time.Now()
		return "", nil, false
	})

	p.Post(func() (string, any, bool) {
		return "time", time.Since(t.start), true
	})
}
