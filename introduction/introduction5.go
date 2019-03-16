package main

import (
	"fmt"
)

const (
	Monday = iota
	Tuesday
	Wednesday = 100 * iota
	Thursday  = iota
	Friday
)

const a = 123456789123456789012345678901234567891234567890e1000
const b = a / 1e1000

var c float64 = b

func main() {
	fmt.Println(c)
	fmt.Println(Monday)
	fmt.Println(Tuesday)
	fmt.Println(Wednesday)
	fmt.Println(Thursday)
	fmt.Println(Friday)
}
