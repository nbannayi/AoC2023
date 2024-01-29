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
        static List<Hailstone> ParseInput(string inputFile)
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
        static List<(Hailstone,Hailstone)> GetHailstonePairs(List<Hailstone> hailstones)
        {
            List<(Hailstone, Hailstone)> pairs = new List<(Hailstone, Hailstone)>();
            for (int i = 0; i < hailstones.Count - 1; i++)
                for (int j = i + 1; j < hailstones.Count; j++)
                    pairs.Add((hailstones[i], hailstones[j]));
            return pairs;            
        }

        static void Main(string[] args)
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
        }
    }
}
