package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"time"
)

func printInLoop(mess string, limit int) {
	fmt.Println("START")
	for i := 0; i < limit; i++ {
		fmt.Println(mess+" ", i)
	}
}

func printFromChannel(c chan string) {
	for true {
		mess := <-c
		fmt.Println(mess)
	}
}

func sendMessages(c chan string, msg string, timestamp int) {
	for true {
		c <- msg + strconv.Itoa(rand.Intn(1000))
		time.Sleep(time.Duration(rand.Intn(500)) * time.Millisecond)
		//<-time.After(time.Duration(rand.Intn(500)) * time.Millisecond)
	}
}

func main() {
	// go printInLoop("Routine 1",20)
	// go printInLoop("Routine 2",20)
	// fmt.Scanln()
	channel1 := make(chan string)
	//go func() {channel1 <- "mess"}()
	go printFromChannel(channel1)
	go sendMessages(channel1, "Message", 20)
	fmt.Scanln()
}
