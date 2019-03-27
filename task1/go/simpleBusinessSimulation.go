package main

import (
	"bufio"
	"errors"
	"fmt"
	"math/rand"
	"os"
	// "sync"
	"time"
	. "constants"
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
}

type product struct {
	value float64
}

type readOperation struct {
	response chan string
}

func inform(message ...interface{}) {
	if mode == TALKATIVE {
		fmt.Println(message...)
	}
}

func doTask(tsk task) (float64, error) {
	switch tsk.operator {
	case "+":
		return tsk.first + tsk.second, nil
	case "-":
		return tsk.first - tsk.second, nil
	case "*":
		return tsk.first * tsk.second, nil
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

func employee(tasks <-chan task, storageChannel chan<- product) {
	for {
		tsk := <-tasks
		res, err := doTask(tsk)
		if err != nil {
			inform(err.Error() + "EMPLOYEE: Something went wrong in employee routine.")
		} else {
			storageChannel <- product{value: res}
			inform("EMPLOYEE: I've done my task! Result is:", res)
		}
		time.Sleep(EMPLOYEE_SLEEP)
	}
}

func getChannelIfCondition(predicate bool, channel <-chan product) <-chan product {
	if predicate {
		return channel
	}
	return nil
}

func storageManager(storageChannel <-chan product, storage *[]product, display chan<- product, storageReadChannel <-chan *readOperation) {
	for {
		if len(*storage) > 0 {
			index := rand.Intn(len(*storage)) // [0,len)
			select {
			case display <- (*storage)[index]:
				inform("STORAGE MANAGER: Product sent to display.")
				*storage = append((*storage)[:index], (*storage)[index+1:]...) //remove from storage
			case newProduct := <-getChannelIfCondition(len(*storage) < MAX_STORAGE_CAPACITY, storageChannel):
				inform("STORAGE MANAGER: Adding new product to the storage.")
				*storage = append(*storage, newProduct)
			case readOp := <-storageReadChannel:
				readOp.response <- fmt.Sprint(*storage)
			}
		} else {
			select {
			case newProduct := <-storageChannel:
				inform("STORAGE MANAGER: Adding new product to the storage.")
				*storage = append(*storage, newProduct)
			case readOp := <-storageReadChannel:
				readOp.response <- fmt.Sprint(*storage)
			}
		}//5 nie 6 tak 7 tak 8 tak
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
			case readOp := <- taskReadChannel:
				readOp.response <- fmt.Sprint(*taskList)
			}
		} else {
			select {
			case newTask := <-tasks:
				*taskList = append(*taskList, newTask)
				inform("TASKS MANAGER: Adding new task to the tasklist.")
			case readOp := <- taskReadChannel:
				readOp.response <- fmt.Sprint(*taskList)
			}
		}
	}
}

func client(displayOfProducts <-chan product) {
	for {
		inform("CLIENT: I am waiting for my product.")
		myProduct := <-displayOfProducts
		inform("CLIENT: Product taken from display, product value: ", myProduct.value)
		time.Sleep(CLIENT_SLEEP)
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
	tasksChannelIn := make(chan task)
	tasksChannelOut := make(chan task)
	productChannel := make(chan product)
	tasksReadChannel := make(chan *readOperation)
	storageReadChannel := make(chan *readOperation)
	display := make(chan product)

	go tasksManager(tasksChannelIn, &tasks, tasksChannelOut, tasksReadChannel)
	go storageManager(productChannel, &storage, display, storageReadChannel)

	for i := 0; i < MAX_CHAIRMEN; i++ {
		go chairman(tasksChannelIn)
	}

	for i := 0; i < MAX_EMPLOYEES; i++ {
		go employee(tasksChannelOut, productChannel)
	}

	for i := 0; i < MAX_CLIENTS; i++ {
		go client(display)
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
