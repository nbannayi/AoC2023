import java.io.File

/**
 * Contraption class for Advent of Code day 16.
 */
class Contraption(inputFile: String, startingPoint: Point) {

    private val inputLines = File(inputFile).readLines()
    private var beams: List<Beam> = listOf()
    var visitedPoints : MutableSet<Point> = mutableSetOf()

    // Store contraption grid layout.
    private val grid = inputLines.map { inputLine ->
        inputLine.toCharArray()
    }.toTypedArray()

    // Add first point of beam.
    init {
        beams += Beam(startingPoint.x, startingPoint.y, startingPoint.dir)
        beams[0].points += listOf(startingPoint)
        visitedPoints.add(startingPoint)
    }

    // Reset all state at a given starting point.
    fun reset(startingPoint: Point) {
        beams = listOf()
        beams += Beam(startingPoint.x, startingPoint.y, startingPoint.dir)
        visitedPoints = mutableSetOf()
        beams[0].points += listOf(startingPoint)
        visitedPoints.add(startingPoint)
    }

    // Return all points around grid for part 2.
    fun getAllStartingPoints() : MutableList<Point> {
        var points : MutableList<Point> = mutableListOf()
        for (c in grid[0].indices) {
            points.add(Point(c,0,Direction.SOUTH))
            points.add(Point(c,grid.indices.last,Direction.NORTH))
        }
        for (r in grid.indices) {
            points.add(Point(0,r,Direction.EAST))
            points.add(Point(grid[0].indices.last, r,Direction.WEST))
        }
        return points
    }

    // Display the contents of the grid.
    fun displayGrid() {
        println("Grid:")
        for (row in grid) {
            println(row.joinToString(""))
        }
    }

    // Display the path at current point in time.
    fun displayPath() {
        println("GridPath:")
        val grid2 = grid.clone()
        for (beam in beams) {
            for (point in beam.points) {
                grid2[point.y][point.x] = '#'
            }
        }
        for (row in grid2) {
            println(row.joinToString(""))
        }
    }

    // Get no. of energised tiles.
    fun getNoEnergisedTiles() : Int {
        val grid2 = grid.clone()
        for (beam in beams) {
            for (point in beam.points) {
                grid2[point.y][point.x] = '#'
            }
        }
        var count = 0
        for (row in grid2.indices) {
            for (col in grid2[row].indices) {
                if (grid2[row][col] == '#') {
                    count++
                }
            }
        }
        return count
    }

    // Move the beam 1 step through contraption. Note: the position is processed then the beam is advanced.
    // If no further moves are possible return true, otherwise returns false.
    fun step() {
        // Process current position of all beams and adjust rotation, any split beams will
        // get added to newBeams.
        var newBeams: List<Beam> = listOf()
        for (beam in beams) {
            val x = beam.x
            val y = beam.y
            val gridElem = grid[y][x]
            val newDirection = when {
                // Right reflector.
                gridElem == '/' && beam.direction == Direction.NORTH -> Direction.EAST
                gridElem == '/' && beam.direction == Direction.SOUTH -> Direction.WEST
                gridElem == '/' && beam.direction == Direction.EAST -> Direction.NORTH
                gridElem == '/' && beam.direction == Direction.WEST -> Direction.SOUTH
                // Left reflector.
                gridElem == '\\' && beam.direction == Direction.NORTH -> Direction.WEST
                gridElem == '\\' && beam.direction == Direction.SOUTH -> Direction.EAST
                gridElem == '\\' && beam.direction == Direction.EAST -> Direction.SOUTH
                gridElem == '\\' && beam.direction == Direction.WEST -> Direction.NORTH
                // Vertical splitter.
                gridElem == '|' && (beam.direction == Direction.EAST || beam.direction == Direction.WEST) -> Direction.SOUTH
                // Horizontal splitter.
                gridElem == '-' && (beam.direction == Direction.NORTH || beam.direction == Direction.SOUTH) -> Direction.WEST
                else -> beam.direction
            }
            beam.newDirection = newDirection
            if (gridElem == '|' && beam.newDirection == Direction.SOUTH) {
                val point = Point(x,y-1, Direction.NORTH)
                if (!visitedPoints.contains(point) && y-1 >= 0) {
                    val newBeam = Beam(x, y - 1, Direction.NORTH)
                    visitedPoints.add(point)
                    newBeam.points += listOf(point)
                    newBeams += listOf(newBeam)
                }
            } else if (gridElem == '-' && beam.newDirection == Direction.WEST) {
                val point = Point(x+1,y, Direction.EAST)
                if (!visitedPoints.contains(point) && x+1 <= grid[0].indices.last) {
                    val newBeam = Beam(x+1,y,Direction.EAST)
                    visitedPoints.add(point)
                    newBeam.points += listOf(point)
                    newBeams += listOf(newBeam)
                }
            }
        }
        // Now advance all beams one position.
        for (beam in beams) {
            val x = beam.x
            val y = beam.y
            val newPosition = when (beam.newDirection) {
                Direction.NORTH -> Point(x,y-1, Direction.NORTH)
                Direction.SOUTH -> Point(x,y+1, Direction.SOUTH)
                Direction.EAST -> Point(x+1,y, Direction.EAST)
                Direction.WEST -> Point(x-1,y, Direction.WEST)
            }
            if (newPosition.x >= 0 && newPosition.x <= grid[0].indices.last && newPosition.y >= 0 &&
                newPosition.y <= grid.indices.last) {
                visitedPoints.add(newPosition)
                beam.points += listOf(newPosition)
                beam.x = newPosition.x
                beam.y = newPosition.y
                beam.direction = beam.newDirection
            }
        }
        // Add newBeams into beams.
        beams += newBeams
    }
}