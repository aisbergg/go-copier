package copier

import (
	"fmt"
	"reflect"
	"strings"
)

// CopierError represents an error that occurred during copying.
type CopierError struct {
	bstTyp reflect.Type
	key    []string
	msg    string
	cause  error
}

func newError(format string, args ...interface{}) *CopierError {
	return &CopierError{
		key: make([]string, 0, 8),
		msg: fmt.Sprintf(format, args...),
	}
}

func wrapError(err error, format string, args ...interface{}) *CopierError {
	return &CopierError{
		key:   make([]string, 0, 8),
		msg:   fmt.Sprintf(format, args...),
		cause: err,
	}
}

// Error returns the formatted error message.
func (e *CopierError) Error() string {
	if len(e.key) > 0 {
		key := e.key[len(e.key)-1]
		for i := len(e.key) - 2; i >= 0; i-- {
			if strings.HasPrefix(e.key[i], "[") {
				key += e.key[i]
			} else {
				key += "." + e.key[i]
			}
		}
		if e.bstTyp != invalidType {
			prf := "(" + e.bstTyp.String() + ")"
			if len(key) > 0 {
				key = prf + "." + key
			} else {
				key = prf
			}
		}
		return fmt.Sprintf("%s: %s", key, e.msg)
	}
	return e.msg
}

// Unwrap returns the underlying error.
func (e *CopierError) Unwrap() error {
	return e.cause
}
