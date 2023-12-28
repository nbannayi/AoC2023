// Advent of Code 2023, Day 16 - The Floor Will Be Lava
// Kotlin.

fun main() {
    val inputFile = "src/main/kotlin/Day16Input.txt"
    val contraption = Contraption(inputFile)
    for (i in 1..1000) {
        contraption.step()
    }
    println("Part 1 answer: ${contraption.getNoEnergisedTiles()}")
}
