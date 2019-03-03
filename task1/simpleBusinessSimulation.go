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
	MAX_CLIENTS          = 40
	MAX_TASKLIST_SIZE    = 40
	MAX_STORAGE_CAPACITY = 40
	EMPLOYEE_SLEEP       = 500 * time.Millisecond
	CHAIRMAN_SLEEP       = 500 * time.Millisecond
	CLIENT_SLEEP         = 500 * time.Millisecond
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
			operator: operators[rand.Intn(2)]}
		fmt.Println("CHAIRMAN: I've made up a new task! Trying to add it to task list.")
		tasks<-newTask
		fmt.Println("CHAIRMAN: I've added a new task to the task list.")
		time.Sleep(CHAIRMAN_SLEEP)
	}
}

func employee(tasks <-chan task, storageChannel chan<- product) {
	for {
		tsk := <-tasks
		res, err := doTask(tsk)
		fmt.Println("EMPLOYEE: I've done my task! Result is:",res)
		if err != nil {
			fmt.Println(err.Error()+"Something went wrong in employee routine.")
		} else {
			storageChannel<-product{value: res}
		}
		time.Sleep(EMPLOYEE_SLEEP)
	}
}

func storageManager(storageChannel chan<- product, storage *[]product, display chan<- product) {
	for {
		
	}
}

func tasksManager(tasks <-chan task, taskList *[]task, taskOutChannel chan<- task) {
	for {

	}
}

func client(displayOfProducts <-chan product) {
	myProduct := <-displayOfProducts
	fmt.Println("CLIENT: Product taken from display, product value: ",myProduct.value)
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	fmt.Println(MAX_EMPLOYEES)
}
