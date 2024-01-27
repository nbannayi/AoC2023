package aoc2023.day23;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * Represents a trail map for Advent of Code Day 2023.
 */
public class TrailMap {
    private char[][] trailGrid;
    private char[][] visitedGrid;
    private char[][] outputGrid;
    private int longestPathLength;

    /**
     * Construct a trail map object.
     * @param inputFile file containing grid to traverse.
     */
    public TrailMap(String inputFile) {
        // First get all input lines into a list.
        File file = new File(inputFile);
        var inputLines = new ArrayList<String>();
        int noRows = 0;
        try (Scanner scanner = new Scanner(file)) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                inputLines.add(line);
                noRows++;
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
        // Now create all grid arrays.
        var noCols = inputLines.getFirst().length();
        trailGrid = new char[noRows][noCols];
        visitedGrid = new char[noRows][noCols];
        outputGrid = new char[noRows][noCols];
        for (var row = 0; row < noRows; row++) {
            var line = inputLines.get(row);
            for (var col = 0; col < noCols; col++) {
                trailGrid[row][col] = line.charAt(col);
                visitedGrid[row][col] = line.charAt(col);
                outputGrid[row][col] = line.charAt(col);
            }
        }
        longestPathLength = 0;
    }

    // Get a block at a specific point.
    private char getBlock(char[][] grid, Point p) {
        return grid[p.y][p.x];
    }

    // Set a block at a specific point.
    private void setBlock(char[][] grid, Point p, char val) {
        grid[p.y][p.x] = val;
    }

    // Get neighbours of passed point.
    private ArrayList<Point> getNeighbouringPoints(Point current) {
        ArrayList<Point> neighbours = new ArrayList<Point>();
        var e = new Point(current.x+1,current.y);
        var w = new Point(current.x-1,current.y);
        var n = new Point(current.x,current.y-1);
        var s = new Point(current.x,current.y+1);
        var currentBlock = getBlock(trailGrid, current);
        if (currentBlock == '>') {
            neighbours.add(e);
        } else if (currentBlock == '<') {
            neighbours.add(w);
        } else if (currentBlock == '^') {
            neighbours.add(n);
        } else if (currentBlock == 'v') {
            neighbours.add(s);
        } else {
            if (current.y-1 >= 0 && (getBlock(trailGrid, n) == '.' || getBlock(trailGrid, n) == '^'))
                neighbours.add(n);
            if (current.y+1 < trailGrid.length && (getBlock(trailGrid, s) == '.' || getBlock(trailGrid, s) == 'v'))
                neighbours.add(s);
            if (current.x+1 >= 0 && (getBlock(trailGrid, e) == '.' || getBlock(trailGrid, e) == '>'))
                neighbours.add(e);
            if (current.x-1 < trailGrid[0].length && (getBlock(trailGrid, w) == '.' || getBlock(trailGrid, w) == '<'))
                neighbours.add(w);
        }
        return neighbours;
    }

    /**
     * Walk around the map (scenically)
     * @param cur start position
     * @param dest end position
     * @param counter initial counter
     */
    public void walk(Point cur, Point dest, int counter) {
        // Out of bounds.
        if (cur.x < 0 || cur.x >= trailGrid[0].length || cur.y < 0 || cur.y >= trailGrid.length) return;
        // Got to destination.
        if (cur.x == dest.x && cur.y == dest.y) {
            if (longestPathLength < counter) {
                longestPathLength = counter;
                for (var row = 0; row < trailGrid.length; row++)
                    for (var col = 0; col < trailGrid[0].length; col++)
                        outputGrid[row][col] = visitedGrid[row][col];
                setBlock(outputGrid, cur, 'O');
            }
        }
        // Already visited.
        if (getBlock(visitedGrid, cur) == 'O') return;
        // Now recursively walk.
        if (getBlock(trailGrid, cur) != '#') {
            setBlock(visitedGrid,cur,'O');
            ArrayList<Point> neighbouringPoints = getNeighbouringPoints(cur);
            for (var np : neighbouringPoints)
                walk(np, dest, counter + 1);
            setBlock(visitedGrid,cur,getBlock(trailGrid,cur));
        }
    }

    /**
     * Display map.
     * @param option 1 = map, 2 = output longest path
     */
    public void display(int option) {
        for (var row = 0; row < trailGrid.length; row++) {
            for (var col = 0; col < trailGrid[0].length; col++) {
                if (option == 1)
                    System.out.print(trailGrid[row][col]);
                else
                    System.out.print(outputGrid[row][col]);
            }
            System.out.println();
        }
    }

    /**
     * Get longest path length.
     * @return longest path length.
     */
    public int getLongestPathLength() {
        return longestPathLength-1;
    }

    /**
     * Flatten all slopes for part 2.
     */
    public void flattenSlopes() {
        for (var row = 0; row < trailGrid.length; row++)
            for (var col = 0; col < trailGrid[0].length; col++)
                if (trailGrid[row][col] != '#')
                    trailGrid[row][col] = '.';
    }
}