package main

import (
	"fmt"
	"math/rand"
	"time"
)

const (
	numOfWorkers = 30
)

func doWork(done chan<- bool, storageChannel chan<- int, data int) { //indicate that we will only write to this channel
	storageChannel <- data * data
	time.Sleep(250 * time.Millisecond)
	fmt.Println("I have done my work for data:", data)
	done <- true
}

func emptyStorage(arr *[]int) {
	*arr = make([]int, 0)
}

func manageStorage(storage *[]int, storageChannel <-chan int,doneChannel chan<- bool) {
	// for { //true
	// 	if val, isValid := <-storageChannel; isValid {
	// 		*storage = append(*storage, v)
	// 	} else {
	// 		return
	// 	}
	// }
	for v := range storageChannel {
		*storage = append(*storage, v)
	}
	doneChannel <- true
}

func main() {
	doneChannel := make(chan bool, numOfWorkers) //workers
	storageChannel := make(chan int, 3)
	storage := make([]int, 0, numOfWorkers)

	rand.Seed(time.Now().UTC().UnixNano())

	go manageStorage(&storage, storageChannel,doneChannel)
	for i := 0; i < numOfWorkers; i++ {
		go doWork(doneChannel, storageChannel, rand.Intn(1000))
	}
	for i := 0; i < numOfWorkers; i++ {
		<-doneChannel
	}
	close(storageChannel)
	<-doneChannel
	//reading from storage

	fmt.Println(len(storage), storage)
}
