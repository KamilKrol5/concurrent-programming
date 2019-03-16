package main

import (
	"errors"
	"fmt"
	"math/rand"
	"time"
)

type cuttable interface {
	cutDown() error
}

type plantable interface {
	plant()
}

type tree struct {
	height   int
	diameter int
	name     string
}

func (t *tree) cutDown() error {
	rand.Seed(time.Now().UTC().UnixNano())
	newHeight := 5 + rand.Intn(15)
	if t.height < newHeight {
		return errors.New("This tree is already cut down!")
	} else {
		fmt.Println(t.name + ": I'am cut down!")
		t.height = newHeight
		return nil
	}
}

func (t *tree) takeCare() {
	fmt.Println(t.name + " is growing far better!")
}

type flower struct {
	color string
	name  string
}

func (f *flower) cutDown() error {
	fmt.Println(f.name + ": I'am cut down!")
	return nil
}

func pepareMeadow(c []cuttable) {
	for _, v := range c {
		v.cutDown()
	}
}
func main() {
	// Go’s structs are typed collections of fields. They’re useful for grouping data together to form records.
	fmt.Println(tree{420, 46, "brzoza Basia"})
	fmt.Printf("%v\n", tree{diameter: 46, name: "jodła Joasia"})
	fmt.Printf("%#v\n", tree{height: 120, name: "wierzba Wiktoria"})
	fmt.Println(tree{})

	t := tree{name: "sosna Sylwia", height: 523, diameter: 67}
	fmt.Println(t)
	t.takeCare()
	t2 := &t
	for i := 0; i < 5; i++ {
		if e := t.cutDown(); e != nil {
			fmt.Println(e.Error())
		}
	}

	fmt.Println(t, t2, *t2)

	pepareMeadow([]cuttable{&tree{name: "modrzew Mariusz", height: 412, diameter: 45}, &flower{color: "red", name: "aster Andrzej"}})
	//var tt1, tt2, tt3 cuttable = t, t2, tree{}

	(&tree{}).takeCare()
}
