package main

import (
	"bufio"
	"errors"
	"fmt"
	"math/rand"
	"os"
	"sync"
	. "constants"
	"time"
)

var operators = [3]string{"+", "-", "*"}
var mode = CALM

type task struct {
	first    float64
	second   float64
	operator string
	result   *float64
}

type product struct {
	value float64
}

type machine struct {
	operation         string
	status			  int
	taskSourceChannel chan *task
	resultChannel     chan *task
	backDoor		  chan int
}

type employee struct {
	isPatient        bool
	numberOfTaskDone int
	lock sync.Mutex
}

type serviceMan struct {
	id     int
	isFree bool
}

type report struct {
	targetMachine *machine
	machineIndex   int
}

type fixReport struct {
	targetMachine *machine
	machineIndex   int
	whoFixed	   *serviceMan
}

type service struct {
	serviceMen       []serviceMan
	reportChannel    chan *report
	fixReportChannel chan *fixReport
}

type readOperation struct {
	response chan string
}

type getProductOperation struct {
	display chan product
}

func inform(message ...interface{}) {
	if mode == TALKATIVE {
		fmt.Println(message...)
	}
}

func doTask(tsk *task) (*float64, error) {
	switch tsk.operator {
	case "+":
		x := tsk.first + tsk.second
		tsk.result = &x
		return tsk.result, nil
	case "-":
		x := tsk.first - tsk.second
		tsk.result = &x
		return tsk.result, nil
	case "*":
		x := tsk.first * tsk.second
		tsk.result = &x
		return tsk.result, nil
	default:
		xd := 0.0
		return &xd, errors.New("Wrong operator provided: " + tsk.operator)
	}
}

func chairman(tasks chan<- task) {
	for {
		newTask := task{
			first:    rand.Float64() * MAX_ARGUMENT_VALUE,
			second:   rand.Float64() * MAX_ARGUMENT_VALUE,
			operator: operators[rand.Intn(len(operators))]}
		inform("CHAIRMAN: I've made up a new task! Trying to add it to task list.", newTask)
		tasks <- newTask
		inform("CHAIRMAN: I've added a new task to the task list.")
		time.Sleep(CHAIRMAN_SLEEP)
	}
}

func (em *employee) employee(tasks <-chan task, storageChannel chan<- product, availableMachines map[string][]machine, serviceChannel chan<- *report ) {
	em.numberOfTaskDone = 0
	for {
		tsk := <-tasks
		var taskDone *task
		// res, err := doTask(&tsk)
		if em.isPatient {
			taskIsDone := false
			for !taskIsDone {
				randIndex := rand.Intn(len(availableMachines[tsk.operator]))
				chosenMachine := &availableMachines[tsk.operator][randIndex]
				chosenMachine.taskSourceChannel <- &tsk
				inform("PATIENT EMPLOYEE: I put my task into the machine.")
				taskDone = <-chosenMachine.resultChannel
				if (taskDone.result == nil) {
					inform("PATIENT EMPLOYEE: I got an empty result. Machine must be broken.")
					serviceChannel <- &report{targetMachine: chosenMachine, machineIndex: randIndex}
				} else {
					inform("PATIENT EMPLOYEE: I got a result.")
					taskIsDone = true
				}
			}
		} else {
			taskIsDOne := false
			for !taskIsDOne {
				randIndex := rand.Intn(len(availableMachines[tsk.operator]))
				chosenMachine := &availableMachines[tsk.operator][randIndex]
				inform("IMPATIENT EMPLOYEE: Waiting for a machine.")
				select {
				case chosenMachine.taskSourceChannel <- &tsk:
					inform("IMPATIENT EMPLOYEE: I put my task into the machine.")
					taskDone = <-chosenMachine.resultChannel
					if (taskDone.result == nil) {
						inform("IMPATIENT EMPLOYEE: I got an empty result. Machine must be broken.")
						serviceChannel <- &report{targetMachine: chosenMachine, machineIndex: randIndex}
					} else {
						inform("IMPATIENT EMPLOYEE: I got a result.")
						taskIsDOne = true
					}
				case <-time.After(IMPATIENT_WAIT):
					inform("IMPATIENT EMPLOYEE: I'll try another machine.")
				}
			}
		}

		storageChannel <- product{value: *taskDone.result }
		inform("EMPLOYEE: I've done my task! Result is:", taskDone.result)
		em.incrementNumberOfTaskDone()
		time.Sleep(EMPLOYEE_SLEEP)
	}
}

func (empl *employee) incrementNumberOfTaskDone() {
	empl.lock.Lock()
	defer empl.lock.Unlock()

	empl.numberOfTaskDone++
}

func (empl *employee) getNumberOfTasksDone() int {
	empl.lock.Lock()
	defer empl.lock.Unlock()

	return empl.numberOfTaskDone
}

func getChannelIfCondition(predicate bool, channel <-chan product) <-chan product {
	if predicate {
		return channel
	}
	return nil
}

func getGetProductOperationChannelIfCondition(predicate bool, channel <-chan *getProductOperation) <-chan *getProductOperation {
	if predicate {
		return channel
	}
	return nil
}

func storageManager(storageChannel <-chan product, storage *[]product,
	getProductChannel <-chan *getProductOperation, storageReadChannel <-chan *readOperation) {
	for {
		select {
		case getProductOp := <-getGetProductOperationChannelIfCondition(len(*storage) > 0, getProductChannel):
			index := rand.Intn(len(*storage)) // [0,len)
			getProductOp.display <- (*storage)[index]
			inform("STORAGE MANAGER: Product sent to display.")
			*storage = append((*storage)[:index], (*storage)[index+1:]...) //remove from storage
		// case display <- (*storage)[index]:
		// 	inform("STORAGE MANAGER: Product sent to display.")
		// 	*storage = append((*storage)[:index], (*storage)[index+1:]...) //remove from storage
		case newProduct := <-getChannelIfCondition(len(*storage) < MAX_STORAGE_CAPACITY, storageChannel):
			inform("STORAGE MANAGER: Adding new product to the storage.")
			*storage = append(*storage, newProduct)
		case readOp := <-storageReadChannel:
			readOp.response <- fmt.Sprint(*storage)
		}
	}
}

func maybeOutTaskChannel(predicate bool, channel chan<- task) chan<- task {
	if predicate {
		return channel
	}
	return nil
}

func maybeInTaskChannel(predicate bool, channel <-chan task) <-chan task {
	if predicate {
		return channel
	}
	return nil
}

func tasksManager(tasks <-chan task, taskList *[]task, taskOutChannel chan<- task, taskReadChannel <-chan *readOperation) {
	for {
		if len(*taskList) > 0 {
			select {
			case newTask := <-maybeInTaskChannel(len(*taskList) < MAX_TASKLIST_SIZE, tasks): //tasks
				*taskList = append(*taskList, newTask)
				inform("TASKS MANAGER: Adding new task to the tasklist.")
			case taskOutChannel <- (*taskList)[0]:
				inform("TASKS MANAGER: Sending new task.")
				*taskList = append((*taskList)[:0], (*taskList)[1:]...)
			case readOp := <-taskReadChannel:
				readOp.response <- fmt.Sprint(*taskList)
			}
		} else {
			select {
			case newTask := <-tasks:
				*taskList = append(*taskList, newTask)
				inform("TASKS MANAGER: Adding new task to the tasklist.")
			case readOp := <-taskReadChannel:
				readOp.response <- fmt.Sprint(*taskList)
			}
		}
	}
}

func client(getProductRequests chan<- *getProductOperation) {
	for {
		inform("CLIENT: I am waiting for my product.")
		display := make(chan product)
		operation := getProductOperation{display}
		getProductRequests <- &operation
		myProduct := <-operation.display
		inform("CLIENT: Product taken from display, product value: ", myProduct.value)
		time.Sleep(CLIENT_SLEEP)
	}
}

func (m *machine) runMachine() {
	for {
		select {
		case tsk := <-m.taskSourceChannel:
			if tsk.operator != m.operation {
				panic("An attempt to perfrom operation which is different from machine operation type.")
			} else {
				if (m.status != BROKEN) {
					_, err := doTask(tsk)
					if err != nil {
						inform(err.Error() + "MACHINE: Something went wrong in machine routine.")
					} else {
						time.Sleep(MACHINE_SLEEP)
						inform("MACHINE: Sending result")
						m.resultChannel <- tsk
					}
					if (rand.Float64() < IMPATIENT_PROBABILITY) {
						m.status = BROKEN
						inform("MACHINE: has broken")
					}
				} else {
					inform("MACHINE BROKEN!")
					m.resultChannel <- tsk
				}
			}
		case mess := <- m.backDoor:
			if (mess == WORKING) {
				m.status = WORKING
				inform("MACHINE: I am fixed!")
			}
		}
	}
}

func (s* service) service(machines map[string][]machine) {
	type mState struct {
		hasManAssigned bool
		status         int
	}
	addMachines := make([]mState, 0, NUMBER_OF_MACHINES)
	substractMachines := make([]mState, 0, NUMBER_OF_MACHINES)
	multiplyMachines := make([]mState, 0, NUMBER_OF_MACHINES)
	machineStates := make(map[string][]mState)
	for h := 0; h < NUMBER_OF_MACHINES; h++ {
		addMachines = append(addMachines, mState{false, WORKING} )
		substractMachines = append(substractMachines, mState{false, WORKING} )
		multiplyMachines = append(multiplyMachines ,mState{false, WORKING} )
	}
	machineStates["+"] = addMachines
	machineStates["-"] = substractMachines
	machineStates["*"] = multiplyMachines
	inform("SERVICE: starting")
	for {
		select {
		case rep := <- s.reportChannel:
			if (!machineStates[rep.targetMachine.operation][rep.machineIndex].hasManAssigned) {
				inform("SERVICE: I have received a report about broken machine")
				machineStates[rep.targetMachine.operation][rep.machineIndex].status = BROKEN
			}
		case rep := <- s.fixReportChannel:
			machineStates[rep.targetMachine.operation][rep.machineIndex].hasManAssigned = false
			machineStates[rep.targetMachine.operation][rep.machineIndex].status = WORKING
			rep.whoFixed.isFree = true
		}
		for k,v := range machineStates {
			for m := range v {
				if (v[m].status == BROKEN) {
					inform("SERVICE: I will try to find free service man")
					loop1: for i := range s.serviceMen {
						man := &s.serviceMen[i]
						if (man.isFree) {
							inform("SERVICE: free service man has been found and will be sent to a broken machine")
							man.isFree = false
							go func(man *serviceMan, op string, mIndex int) {
								machineStates[op][mIndex].hasManAssigned = true
								inform("SERVICE MAN ",man.id, ": was sent to fix a machine" )
								time.Sleep(SERVICE_MAN_SLEEP)
								inform("SERVICE MAN ",man.id, ": is fixing a machine" )
								machines[op][mIndex].backDoor <- WORKING
								s.fixReportChannel <- &fixReport{ targetMachine: &machines[op][mIndex], machineIndex: mIndex, whoFixed: man }
							}(man, k, m)
							break loop1
						}
					}
				}
			}
		}
	}
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Wrong number of arguments. Valid input is calm|-c|talkative|-t")
		return
	}
	arg := os.Args[1]
	switch arg {
	case "-t", "talkative":
		mode = TALKATIVE
	case "-c", "calm":
		mode = CALM
	default:
		fmt.Println("ERROR:", arg, " is not valid argument.")
	}
	scanner := bufio.NewScanner(os.Stdin)
	// var mutexStorage = &sync.Mutex{}
	// var mutexTasks = &sync.Mutex{}
	rand.Seed(time.Now().UTC().UnixNano())
	fmt.Println(MAX_EMPLOYEES)
	tasks := make([]task, 0, MAX_TASKLIST_SIZE)
	storage := make([]product, 0, MAX_STORAGE_CAPACITY)
	employees := make([]*employee, 0, MAX_EMPLOYEES)
	addMachines := make([]machine, 0, NUMBER_OF_MACHINES)
	substractMachines := make([]machine, 0, NUMBER_OF_MACHINES)
	multiplyMachines := make([]machine, 0, NUMBER_OF_MACHINES)
	machines := make(map[string][]machine)

	tasksChannelIn := make(chan task)
	tasksChannelOut := make(chan task)
	productChannel := make(chan product)
	tasksReadChannel := make(chan *readOperation)
	storageReadChannel := make(chan *readOperation)
	getProductChannel := make(chan *getProductOperation)

	for h := 0; h < NUMBER_OF_MACHINES; h++ {
		addMachines = append(addMachines, machine{operation: "+",
			status:            WORKING,
			taskSourceChannel: make(chan *task),
			resultChannel:     make(chan *task),
			backDoor:          make(chan int)})
		substractMachines = append(substractMachines, machine{operation: "-",
			status:            WORKING,
			taskSourceChannel: make(chan *task),
			resultChannel:     make(chan *task),
			backDoor:          make(chan int)})
		multiplyMachines = append(multiplyMachines, machine{operation: "*",
			status:            WORKING,
			taskSourceChannel: make(chan *task),
			resultChannel:     make(chan *task),
			backDoor:          make(chan int)})
		go addMachines[h].runMachine()
		go substractMachines[h].runMachine()
		go multiplyMachines[h].runMachine()
	}

	machines["+"] = addMachines
	machines["-"] = substractMachines
	machines["*"] = multiplyMachines

	serviceMen := make([]serviceMan, 0, SERVICE_MAN_COUNT)

	for i := 0; i < SERVICE_MAN_COUNT; i++ {
		serviceMen = append(serviceMen, serviceMan{id: i, isFree: true})
	}

	serv := service{
		serviceMen: serviceMen,
		reportChannel:    make(chan *report),
		fixReportChannel: make(chan *fixReport),
	}

	go serv.service(machines)

	go tasksManager(tasksChannelIn, &tasks, tasksChannelOut, tasksReadChannel)
	go storageManager(productChannel, &storage, getProductChannel, storageReadChannel)

	for i := 0; i < MAX_CHAIRMEN; i++ {
		go chairman(tasksChannelIn)
	}

	for i := 0; i < MAX_EMPLOYEES; i++ {
		emp := employee{isPatient: rand.Float64() < IMPATIENT_PROBABILITY}
		employees = append(employees, &emp)
		go emp.employee(tasksChannelOut, productChannel, machines, serv.reportChannel)
	}

	for i := 0; i < MAX_CLIENTS; i++ {
		go client(getProductChannel)
	}
	fmt.Println("Options:\n    s - show Storage\n    t - show Task list\n    e - show Employee statistics\n    ")
	for scanner.Scan() && mode == CALM {
		line := scanner.Text()
		var operation readOperation
		switch line {
		case "s":
			operation = readOperation{make(chan string)}
			storageReadChannel <- &operation
			fmt.Println(<-operation.response)
		case "t":
			operation = readOperation{make(chan string)}
			tasksReadChannel <- &operation
			fmt.Println(<-operation.response)
		case "e":
			for u := 0; u < MAX_EMPLOYEES; u++ {
				fmt.Println("Employee (isPatient =",employees[u].isPatient ,") has done", employees[u].getNumberOfTasksDone(), "tasks.")
			}
		default:
			fmt.Println("Wrong option.\nOptions:\n    s - show Storage\n    t - show Task list\n    e - show Employee statistics\n    ")
		}
	}
}
