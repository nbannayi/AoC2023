// Advent of Code 2023, Day 16 - The Floor Will Be Lava
// Kotlin.

fun main() {
    val inputFile = "src/main/kotlin/Day16Input.txt"
    val startingPoint = Point(0,0,Direction.EAST)
    val contraption = Contraption(inputFile, startingPoint)

    // Part 1.
    for (i in 1..1000) {
        contraption.step()
    }
    println("Part 1 answer: ${contraption.getNoEnergisedTiles()}")

    // Part 2.
    val startingPoints = contraption.getAllStartingPoints()
    var maxEnergised = 0;
    for (startingPoint2 in startingPoints) {
        val contraption2 = Contraption(inputFile, startingPoint2)
        for (i in 1..1000) {
            contraption2.step()
        }
        val energised = contraption2.getNoEnergisedTiles()
        if (energised > maxEnergised) {
            maxEnergised = energised
        }
    }
    println("Part 2 answer: $maxEnergised")
}
