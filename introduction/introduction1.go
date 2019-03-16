//configure go on linux
//sudo update-alternatives --install /usr/bin/go go /usr/lib/go-1.10/bin/go 1000

package main

import "fmt"
import "math"
import "time"

const c float64 = 1.0

func main() {
	var a, b int = 56, 60
	var r float64 = c * math.Pi
	fmt.Println("hello world", a+b, a-b, r)

	//array and loop
	var arr0 [8]int
	var arr = [8]int{1, 2, 3}
	for i := 0; i < len(arr); i++ {
		fmt.Println(arr[i], arr0)
	}

	//slices
	s := make([]int, 3)
	fmt.Println("empty slice:", s)
	s = append(s, 7, 8, 9, 19)
	s2 := make([]int, len(s))
	copy(s2, s)
	fmt.Println("s1 and s2: ", s, s2)
	var s3 = s2[2:5]
	var s4 = s2[:4]
	fmt.Println("s3 and s4: ", s3, s4)
	s3[0] = 100
	fmt.Println("s3 and s4: ", s3, s4)
	s5 := []string{"g", "h", "c"}
	fmt.Println(s5)

	s6 := []int{1,2,3,4,5,6,7,8,9,10}
	copy(s6[7:9], s6[3:])
	fmt.Println(s6)

	s7 := make([]int, 5, 10)
	fmt.Println("s7 len, cap:", len(s7), cap(s7))
	s7prime := append(s7, 1,2,3,4)
	s7prime2 := s7[0:9]
	fmt.Println("s7", s7, s7prime, s7prime2)
	s7prime[1] = 100
	fmt.Println("s7", s7, s7prime)
	s7bis := append(s7,9,8,7,6,5,4,3,2,1)
	fmt.Println("s7", s7, s7prime, s7bis)
	s7bis[1] = 200
	fmt.Println("s7", s7, s7prime, s7bis)

	//switch
	t := time.Now()
	switch {
	case t.Hour() < 12:
		fmt.Println("It's before noon")
	default:
		fmt.Println("It's after noon")
	}
}
