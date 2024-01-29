using System;
using System.Collections.Generic;
using System.IO;

/// <summary>
/// Advent of Code 2023 - Day 24 - Never tell me the odds.
/// C#
/// </summary>
namespace Day24
{
    class Program
    {
        // Parse input file and return all hailstones.
        private static List<Hailstone> ParseInput(string inputFile)
        {            
            string[] lines = File.ReadAllLines(inputFile);
            var hailstones = new List<Hailstone>();
            foreach (var line in lines)
            {
                var tokens = line.Replace('@', ',').Replace(" ", "").Split(',');
                long[] inputs = Array.ConvertAll(tokens, long.Parse);
                hailstones.Add(new Hailstone(inputs[0], inputs[1], inputs[2], inputs[3], inputs[4], inputs[5]));
            }
            return hailstones;
        }

        // Create a list containing all unique pairs of hailstones.
        private static List<(Hailstone,Hailstone)> GetHailstonePairs(List<Hailstone> hailstones)
        {
            List<(Hailstone, Hailstone)> pairs = new List<(Hailstone, Hailstone)>();
            for (int i = 0; i < hailstones.Count - 1; i++)
                for (int j = i + 1; j < hailstones.Count; j++)
                    pairs.Add((hailstones[i], hailstones[j]));
            return pairs;            
        }

        // Solve either for px,py,vx,vy or py,pz,vy,vz - pass is xy = true for first,
        // xy = false for second.
        private static (long,long,long,long) SolveAxes(List<(Hailstone,Hailstone)> hailstonePairs, bool xy)
        {
            // Solve x,y.
            var matrix = new double[4, 4];
            var vector = new double[4];
            for (var i = 0; i < 4; i++)
            {
                var (hailstone1, hailstone2) = hailstonePairs[i];
                var coefficients = hailstone1.GetLinearConstraint(hailstone2, xy);
                for (var j = 0; j < 4; j++) matrix[i, j] = coefficients[j];
                vector[i] = coefficients[4];
            }
            var solver = new LinearSolver(matrix, vector);
            solver.Solve();
            var p1 = (long)Math.Round(solver.ValuesVector[0], 0);
            var p2 = (long)Math.Round(solver.ValuesVector[1], 0);
            var v1 = (long)Math.Round(solver.ValuesVector[2], 0);
            var v2 = (long)Math.Round(solver.ValuesVector[3], 0);
            return (p1, p2, v1, v2);
        }

        public static void Main(string[] args)
        {
            // Parse input.
            var inputFile = "Day24Input.txt";
            var hailstonePairs = GetHailstonePairs(ParseInput(inputFile));

            // Part 1.

            // Work out if each pair intersects.
            var bounds = (200000000000000, 400000000000000);
            var noIntersections = 0;
            foreach (var (hailstone1, hailstone2) in hailstonePairs)
            {
                var intersects = hailstone1.IntersectsWithinBounds(hailstone2, bounds);
                if (intersects) noIntersections++;
            }
            Console.WriteLine("Part 1 answer: " + noIntersections);

            // Part 2.

            // Solve x,y.
            var (rockPx, rockPy, _, _) = SolveAxes(hailstonePairs, true);
            // Solve y,z.
            var (_, rockPz, _, _) = SolveAxes(hailstonePairs, false);

            Console.WriteLine($"Part 2 answer: {rockPx+rockPy+rockPz}");
        }
    }
}
