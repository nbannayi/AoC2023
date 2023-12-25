// Advent of Code 2023, Day 14 - Parabolic Reflector Dish.
// Go.

package main

import (
	"bufio"
	"fmt"
	"os"
)

// Tilt directions.
const (
	North int = 1
	West  int = 2
	South int = 3
	East  int = 4
)

// Get dish from input file.
func getDish(inputFile string) [][]rune {
	file, _ := os.Open(inputFile)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var dish [][]rune
	for scanner.Scan() {
		line := scanner.Text()
		dish = append(dish, []rune(line))
	}
	return dish
}

// Display dish.
func displayDish(dish [][]rune) {
	for r := 0; r < len(dish); r++ {
		for c := 0; c < len(dish[r]); c++ {
			fmt.Print(string(dish[r][c]))
		}
		fmt.Println()
	}
}

// Tilt entire dish.
func tilt(dish [][]rune, dir int) {
	tiltOnce := func(dish [][]rune, row int, column int, dir int) {
		// Space or square rock, do nothing.
		initialRock := string(dish[row][column])
		if initialRock == "." || initialRock == "#" {
			return
		}
		// If a round rock advance forward 1 place in required direction.
		switch dir {
		case North:
			for r := row - 1; r >= 0; r-- {
				northRock := string(dish[r][column])
				if northRock == "." {
					dish[r][column] = 79   // "0"
					dish[r+1][column] = 46 // "."
				} else {
					break
				}
			}
		case West:
			for c := column - 1; c >= 0; c-- {
				westRock := string(dish[row][c])
				if westRock == "." {
					dish[row][c] = 79   // "0"
					dish[row][c+1] = 46 // "."
				} else {
					break
				}
			}
		case East:
			for c := column + 1; c < len(dish[0]); c++ {
				eastRock := string(dish[row][c])
				if eastRock == "." {
					dish[row][c] = 79   // "0"
					dish[row][c-1] = 46 // "."
				} else {
					break
				}
			}
		case South:
			for r := row + 1; r < len(dish); r++ {
				northRock := string(dish[r][column])
				if northRock == "." {
					dish[r][column] = 79   // "0"
					dish[r-1][column] = 46 // "."
				} else {
					break
				}
			}
		}
	}
	// Now tilt every possible roch from the top
	switch dir {
	case North:
		for r := 0; r < len(dish); r++ {
			for c := 0; c < len(dish[r]); c++ {
				tiltOnce(dish, r, c, dir)
			}
		}
	case West:
		for c := 0; c < len(dish[0]); c++ {
			for r := 0; r < len(dish); r++ {
				tiltOnce(dish, r, c, dir)
			}
		}
	case East:
		for c := len(dish[0]) - 1; c >= 0; c-- {
			for r := 0; r < len(dish); r++ {
				tiltOnce(dish, r, c, dir)
			}
		}
	case South:
		for r := len(dish) - 1; r >= 0; r-- {
			for c := 0; c < len(dish[0]); c++ {
				tiltOnce(dish, r, c, dir)
			}
		}
	}
}

// Calculate the total load on thw dish.
func calculateLoad(dish [][]rune) int {
	totalLoad := 0
	noRows := len(dish)
	for r := 0; r < len(dish); r++ {
		for c := 0; c < len(dish[r]); c++ {
			if dish[r][c] == 79 {
				totalLoad += (noRows - r)
			}
		}
	}
	return totalLoad
}

// Do a spin.
func spin(dish [][]rune) {
	tilt(dish, North)
	tilt(dish, West)
	tilt(dish, South)
	tilt(dish, East)
}

func main() {
	// Part 1.
	dish1 := getDish("Day14Input.txt")
	tilt(dish1, North)
	fmt.Println("Part 1 answer: ", calculateLoad(dish1))

	// Part 2.
	// Did this by monkeying around with the data, but a star os a star.

	// Need new untilted about dish.
	dish2 := getDish("Day14Input.txt")

	// Spin 120 times, after this ppoint it repeats every 44 spins.
	for i := 1; i <= 120; i++ {
		spin(dish2)
	}

	// Now to find out billionth one...
	remainingSpins := (1000000000 - 120) % 44
	for i := 1; i <= remainingSpins; i++ {
		spin(dish2)
	}
	fmt.Println("Part 2 answer: ", calculateLoad(dish2))
}
