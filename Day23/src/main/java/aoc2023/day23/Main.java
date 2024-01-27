package aoc2023.day23;

// Advent of Code 2023 - Day 23 - A Long Walk.
// Java.
public class Main {
    public static void main(String[] args) {
        var inputFile = "src/main/java/aoc2023/day23/Day23Input.txt";
        var start = new Point(1,0);
        var end = new Point(139,139); // 21, 21 example input, 139, 139 real input

        // Part 1.
        var trailMap1 = new TrailMap(inputFile);
        trailMap1.walk(start,end,0);
        //trailMap.display(2);
        System.out.println("Part 1 answer: "+trailMap1.getLongestPathLength());

        // Part 2.
        // This is slooooooow, takes 15 mins or so.  Can't be bothered to optimise I am burnt out!
        var trailMap2 = new TrailMap(inputFile);
        trailMap2.flattenSlopes();
        trailMap2.walk(start,end,0);
        //trailMap.display(2);
        System.out.println("Part 2 answer: "+trailMap2.getLongestPathLength());
    }
}