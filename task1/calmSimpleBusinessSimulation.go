package main

import (
	"errors"
	"fmt"
	"math/rand"
	"time"
	// "constants"
)

const (
	MAX_ARGUMENT_VALUE   = 500.0
	MAX_EMPLOYEES        = 40
	MAX_CHAIRMEN         = 1
	MAX_CLIENTS          = 5
	MAX_TASKLIST_SIZE    = 40
	MAX_STORAGE_CAPACITY = 40
	EMPLOYEE_SLEEP       = 1000 * time.Millisecond
	CHAIRMAN_SLEEP       = 700 * time.Millisecond
	CLIENT_SLEEP         = 2000 * time.Millisecond
)

var operators = [3]string{"+", "-", "*"}

type task struct {
	first    float64
	second   float64
	operator string
}

type product struct {
	value float64
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
		fmt.Println("CHAIRMAN: I've made up a new task! Trying to add it to task list.",newTask)
		tasks<-newTask
		fmt.Println("CHAIRMAN: I've added a new task to the task list.")
		time.Sleep(CHAIRMAN_SLEEP)
	}
}

func employee(tasks <-chan task, storageChannel chan<- product) {
	for {
		tsk := <-tasks
		res, err := doTask(tsk)
		if err != nil {
			fmt.Println(err.Error()+"EMPLOYEE: Something went wrong in employee routine.")
		} else {
			storageChannel<-product{value: res}
			fmt.Println("EMPLOYEE: I've done my task! Result is:",res)
		}
		time.Sleep(EMPLOYEE_SLEEP)
	}
}

func getChannelIfCondition(predicate bool, channel <-chan product) <-chan product {
	if (predicate) {
		return channel
	}
	return nil
}

func storageManager(storageChannel <-chan product, storage *[]product, display chan<- product) {
	for {
		if len(*storage) > 0 {
			index := rand.Intn(len(*storage)) // [0,len)
			select {
			case display <- (*storage)[index]:
				fmt.Println("STORAGE MANAGER: Product sent to display.")
				*storage = append((*storage)[:index], (*storage)[index+1:]...) //remove from storage
			case newProduct := <-getChannelIfCondition(len(*storage) < MAX_STORAGE_CAPACITY,storageChannel):
				fmt.Println("STORAGE MANAGER: Adding new product to the storage.")
				*storage = append(*storage,newProduct)
			}
		} else {
			select {
			case newProduct := <-storageChannel:
				fmt.Println("STORAGE MANAGER: Adding new product to the storage.")
				*storage = append(*storage,newProduct)
			}
		}
	}
}

func maybeOutTaskChannel(predicate bool, channel chan<-task) chan<-task {
	if (predicate) {
		return channel
	}
	return nil
}

func maybeInTaskChannel(predicate bool, channel <-chan task) <-chan task {
	if (predicate) {
		return channel
	}
	return nil
}

func tasksManager(tasks <-chan task, taskList *[]task, taskOutChannel chan<- task) {
	for {
		if len(*taskList) > 0 {
			select {
			case newTask := <-maybeInTaskChannel(len(*taskList) < MAX_TASKLIST_SIZE, tasks): //tasks
				*taskList = append(*taskList,newTask)
				fmt.Println("TASKS MANAGER: Adding new task to the tasklist.")
			case taskOutChannel <- (*taskList)[0]:
				fmt.Println("TASKS MANAGER: Sending new task.")
				*taskList = append((*taskList)[:0], (*taskList)[1:]...)
			}
		} else {
			select {
			case newTask := <-tasks:
				*taskList = append(*taskList,newTask)
				fmt.Println("TASKS MANAGER: Adding new task to the tasklist.")
			}
		}
	}
}

func client(displayOfProducts <-chan product) {
	fmt.Println("CLIENT: I am waiting for my product.")
	myProduct := <-displayOfProducts
	fmt.Println("CLIENT: Product taken from display, product value: ",myProduct.value)
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	fmt.Println(MAX_EMPLOYEES)
	tasks := make([]task, 0, MAX_TASKLIST_SIZE)
	storage := make([]product, 0, MAX_STORAGE_CAPACITY)
	tasksChannelIn := make(chan task)
	tasksChannelOut := make(chan task)
	productChannel := make(chan product)
	display := make(chan product)

	go tasksManager(tasksChannelIn,&tasks,tasksChannelOut)
	go storageManager(productChannel,&storage,display)

	for i := 0; i < MAX_CHAIRMEN; i++ {
		go chairman(tasksChannelIn)
	}

	for i := 0; i < MAX_EMPLOYEES; i++ {
		go employee(tasksChannelOut,productChannel)
	}

	for i := 0; i < MAX_CLIENTS; i++ {
		go client(display)
	}
	fmt.Scanln()
}
