/**
 * Represents a point in 2D space.
  */
data class Point(val x: Int, val y: Int, val dir: Direction)

/**
 * Represents a direction in 2D space.
 */
enum class Direction {
    NORTH,
    SOUTH,
    EAST,
    WEST
}

/**
 * Beam class to represent all beams in a contraption.
 */
data class Beam(var x: Int = 0, var y: Int = 0, var direction: Direction = Direction.EAST, var newDirection : Direction = Direction.EAST, var points: List<Point> = listOf())