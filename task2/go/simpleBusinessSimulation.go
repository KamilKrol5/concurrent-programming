package main

import (
	"bufio"
	"errors"
	"fmt"
	"math/rand"
	"os"
	// "sync"
	. "constants"
	"time"
)

// const (
// 	MAX_ARGUMENT_VALUE   = 500.0
// 	MAX_EMPLOYEES        = 2
// 	MAX_CHAIRMEN         = 1
// 	MAX_CLIENTS          = 1
// 	MAX_TASKLIST_SIZE    = 40
// 	MAX_STORAGE_CAPACITY = 40
// 	EMPLOYEE_SLEEP       = 1000 * time.Millisecond
// 	CHAIRMAN_SLEEP       = 400 * time.Millisecond
// 	CLIENT_SLEEP         = 2000 * time.Millisecond
// 	CALM                 = iota
// 	TALKATIVE
// )

var operators = [3]string{"+", "-", "*"}
var mode = CALM

type task struct {
	first    float64
	second   float64
	operator string
	result   float64
}

type product struct {
	value float64
}

type machine struct {
	operation         string
	taskSourceChannel chan *task
	resultChannel     chan *task
}

type employee struct {
	isPatient        bool
	numberOfTaskDone int
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

func doTask(tsk *task) (float64, error) {
	switch tsk.operator {
	case "+":
		tsk.result = tsk.first + tsk.second
		return tsk.result, nil
	case "-":
		tsk.result = tsk.first - tsk.second
		return tsk.result, nil
	case "*":
		tsk.result = tsk.first * tsk.second
		return tsk.result, nil
	default:
		return 0, errors.New("Wrong operator provided: " + tsk.operator)
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

func (em *employee) employee(tasks <-chan task, storageChannel chan<- product, availableMachines map[string][]machine) {
	em.numberOfTaskDone = 0
	for {
		tsk := <-tasks
		var taskDone *task
		// res, err := doTask(&tsk)
		if em.isPatient {
			chosenMachine := availableMachines[tsk.operator][rand.Intn(len(availableMachines[tsk.operator]))]
			chosenMachine.taskSourceChannel <- &tsk
			inform("PATIENT EMPLOYEE: I put my task into the machine.")
			taskDone = <-chosenMachine.resultChannel
			inform("PATIENT EMPLOYEE: I got a result.")
		} else {
			taskIsDOne := false
			for !taskIsDOne {
				chosenMachine := availableMachines[tsk.operator][rand.Intn(len(availableMachines[tsk.operator]))]
				select {
				case chosenMachine.taskSourceChannel <- &tsk:
					inform("IMPATIENT EMPLOYEE: I put my task into the machine.")
					taskDone = <-chosenMachine.resultChannel
					inform("IMPATIENT EMPLOYEE: I got a result.")
					taskIsDOne = true
				case <-time.After(IMPATIENT_WAIT):
					inform("IMPATIENT EMPLOYEE: I'll try another machine.")
				}
			}
		}

		storageChannel <- product{value: taskDone.result}
		inform("EMPLOYEE: I've done my task! Result is:", taskDone.result)
		em.numberOfTaskDone++
		time.Sleep(EMPLOYEE_SLEEP)
	}
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
		tsk := <-m.taskSourceChannel
		if tsk.operator != m.operation {
			panic("An attempt to perfrom operation which is different from machine operation type.")
		} else {
			_, err := doTask(tsk)
			if err != nil {
				inform(err.Error() + "MACHINE: Something went wrong in machine routine.")
			} else {
				time.Sleep(MACHINE_SLEEP)
				inform("MACHINE: Sending result")
				m.resultChannel <- tsk
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
	employees := make([]employee, 0, MAX_EMPLOYEES)
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
			taskSourceChannel: make(chan *task),
			resultChannel:     make(chan *task)})
		substractMachines = append(substractMachines, machine{operation: "-",
			taskSourceChannel: make(chan *task),
			resultChannel:     make(chan *task)})
		multiplyMachines = append(multiplyMachines, machine{operation: "*",
			taskSourceChannel: make(chan *task),
			resultChannel:     make(chan *task)})
		go addMachines[h].runMachine()
		go substractMachines[h].runMachine()
		go multiplyMachines[h].runMachine()
	}

	machines["+"] = addMachines
	machines["-"] = substractMachines
	machines["*"] = multiplyMachines

	go tasksManager(tasksChannelIn, &tasks, tasksChannelOut, tasksReadChannel)
	go storageManager(productChannel, &storage, getProductChannel, storageReadChannel)

	for i := 0; i < MAX_CHAIRMEN; i++ {
		go chairman(tasksChannelIn)
	}

	for i := 0; i < MAX_EMPLOYEES; i++ {
		emp := employee{isPatient: rand.Float64() < IMPATIENT_PROBABILITY}
		employees = append(employees, emp)
		go emp.employee(tasksChannelOut, productChannel, machines)
	}

	for i := 0; i < MAX_CLIENTS; i++ {
		go client(getProductChannel)
	}
	fmt.Println("Options:\n    s - show Storage\n    t - show Task list\n    ")
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
		default:
			fmt.Println("Wrong option.\nOptions:\n    s - show Storage\n    t - show Task list\n    ")
		}
	}
}
