package constants

import "time"

const (
	MAX_ARGUMENT_VALUE   = 500.0
	MAX_EMPLOYEES        = 3
	MAX_CHAIRMEN         = 1
	MAX_CLIENTS          = 1
	MAX_TASKLIST_SIZE    = 40
	MAX_STORAGE_CAPACITY = 40
	EMPLOYEE_SLEEP       = 1000 * time.Millisecond
	CHAIRMAN_SLEEP       = 400 * time.Millisecond
	CLIENT_SLEEP         = 2000 * time.Millisecond
	CALM                 = iota
	TALKATIVE
	NUMBER_OF_MACHINES   = 1
	MACHINE_SLEEP        = 1000 * time.Millisecond
	IMPATIENT_WAIT       = 200 * time.Millisecond
	IMPATIENT_PROBABILITY= 0.5
)