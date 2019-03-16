package main

import "fmt"

func main() {
	//maps
	var map1 = make(map[string]float64)
	map1["aaa"] = 4.0
	map1["ccc"] = 5.0
	map1["aaa"] = 1.0
	fmt.Println(map1)
	_, prs := map1["ccc"]
	if prs {
		fmt.Println(prs, ", Nice!")
	} else {
		fmt.Println(prs, ", Sad!")
	}
	map2 := map[string]float64{"a": 1.2, "b": 1.0, "c": 5.9}
	fmt.Println(map2)

	//functions
	fmt.Println(doSth(1, 2, 5.2))
	variadicFunction([]int{1, 4, 89}...)
	fmt.Println([]interface{}{1, 4, 89}, 5)
	fmt.Println([]interface{}{1, 4, 89}...)

	//closures
	// This function intSeq returns another function, which we define anonymously in the body of intSeq.
	// The returned function closes over the variable i to form a closure.
	nextInt := intSeq()
	fmt.Println(nextInt())
	fmt.Println(nextInt())
	fmt.Println(nextInt())
}

//functions
func doSth(a, b int, c float64) (float64, int) {
	return c * (float64)(a*b), a * b
}

func variadicFunction(ints ...int) {
	for _, v := range ints {
		fmt.Println(v)
	}
}

func intSeq() func() int {
	i := 0
	return func() int {
		i++
		return i
	}
}
