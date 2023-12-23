// Advent of Code 2023, Day 13 - Point of Incidence.
// Dart.
import 'dart:io';
import 'Mirror.dart';

void main() {
  // Parse raw input.
  var inputLines = File('Day13Input.txt').readAsStringSync();
  List<String> rawInputs = inputLines.split('\n\n');

  // Load into an array of Mirror objects.
  List<Mirror> mirrors = [];
  for (var rawInput in rawInputs) {
    mirrors.add(Mirror(rawInput.split('\n')));
  }

  // Part 1.
  int summaryNotes = 0;
  for (int i = 0; i < mirrors.length; i++) {
    var reflectionLines = mirrors[i].getReflectionLines();
    for (var rl in reflectionLines) {
      summaryNotes += rl.summarise();
    }
  }
  print('Part 1 answer: $summaryNotes');

  // Part 2.
  int alternateSummaryNotes = 0;
  for (int i = 0; i < mirrors.length; i++) {
    var altenateReflectionLine = mirrors[i].getAlternateReflectionLine();
    alternateSummaryNotes += altenateReflectionLine.summarise();
  }
  print('Part 2 answer: $alternateSummaryNotes');
}
