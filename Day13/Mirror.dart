import 'ReflectionLine.dart';

// Mirror object.
class Mirror {
  List<String> rows;

  Mirror(List<String> inputRows) : rows = inputRows;

  // Display all rows contained in mirror.
  void display() {
    for (var row in this.rows) {
      print(row);
    }
  }

  // Get given row (0 indexed.)
  String getRow(int r) {
    return rows[r];
  }

  // Get given column (0 indexed.)
  String getColumn(int c) {
    var column = '';
    for (var r = 0; r < rows.length; r++) {
      column += rows[r][c];
    }
    return column;
  }

  // Determine if a pair of reflection lines contain a smudge.
  bool linesContainSmudge(ReflectionLine line1, ReflectionLine line2) {
    var diffSum = 0;
    var line1Text = '';
    var line2Text = '';
    if (line1.reflectionType == ReflectionType.horizonal) {
      line1Text = getRow(line1.position);
      line2Text = getRow(line2.position);
    } else {
      line1Text = getColumn(line1.position);
      line2Text = getColumn(line2.position);
    }
    for (int i = 0; i < line1Text.length; i++) {
      if (line1Text[i] != line2Text[i]) diffSum++;
    }
    return diffSum == 1;
  }

  // Find a pair of smudge lines.
  List<ReflectionLine> getSmudgeLines() {
    List<ReflectionLine> foundLines = [];
    // Search horizontal.
    for (int i = 0; i < this.rows.length; i++) {
      for (int j = i + 1; j < this.rows.length; j++) {
        var line1 = ReflectionLine(ReflectionType.horizonal, i);
        var line2 = ReflectionLine(ReflectionType.horizonal, j);
        if (linesContainSmudge(line1, line2)) {
          foundLines.add(line1);
          foundLines.add(line2);
        }
      }
    }
    // Search verticals.
    for (int i = 0; i < this.rows[0].length; i++) {
      for (int j = i + 1; j < this.rows[0].length; j++) {
        var line1 = ReflectionLine(ReflectionType.vertical, i);
        var line2 = ReflectionLine(ReflectionType.vertical, j);
        if (linesContainSmudge(line1, line2)) {
          foundLines.add(line1);
          foundLines.add(line2);
        }
      }
    }
    return foundLines;
  }

  // Given a pair of smudge lines, resturn a new mirror object with correction.
  Mirror updateSmudgeLines(ReflectionLine line1, ReflectionLine line2) {
    List<String> newRows = List.from(rows);
    if (line1.reflectionType == ReflectionType.horizonal) {
      var line1Text = getRow(line1.position);
      newRows[line1.position] = line1Text;
      newRows[line2.position] = line1Text;
    } else {
      var line1Text = getColumn(line1.position);
      for (int i = 0; i < line1Text.length; i++) {
        var newString = newRows[i]
            .replaceRange(line1.position, line1.position + 1, line1Text[i])
            .replaceRange(line2.position, line2.position + 1, line1Text[i]);
        newRows[i] = newString;
      }
    }
    return Mirror(newRows);
  }

  // Get alternate reflectiin line when smudge corrected.
  ReflectionLine getAlternateReflectionLine() {
    var originalReflectionLine = getReflectionLines()[0];
    var originalSummary = originalReflectionLine.summarise();
    var alternateReflectionLine = ReflectionLine(ReflectionType.horizonal, 0);
    var smls = getSmudgeLines();
    for (int i = 0; i <= smls.length - 2; i += 2) {
      var newMirror = updateSmudgeLines(smls[i], smls[i + 1]);
      var rls = newMirror.getReflectionLines();
      for (var rl in rls) {
        if (rl.summarise() != originalSummary) {
          alternateReflectionLine = rl;
          break;
        }
      }
    }
    return alternateReflectionLine;
  }

  // Get reflection line in this mirror.
  List<ReflectionLine> getReflectionLines() {
    var noRows = rows.length;
    List<ReflectionLine> output = [];
    // Try to find horizontals.
    for (var r = 0; r <= noRows - 2; r++) {
      var foundHorizontal = false;
      // If we find two rows the same, check it is a perfect reflection.
      if (getRow(r) == getRow(r + 1)) {
        foundHorizontal = true;
        var offset = 1;
        while (r - offset >= 0 && r + offset + 1 <= noRows - 1) {
          if (getRow(r - offset) != getRow(r + offset + 1)) {
            foundHorizontal = false;
            break;
          }
          offset++;
        }
      }
      if (foundHorizontal) {
        output.add(ReflectionLine(ReflectionType.horizonal, r + 1));
      }
    }

    // Try to find verticals (assume nothing!)
    var noCols = rows[0].length;
    for (var c = 0; c <= noCols - 2; c++) {
      var foundVertical = false;
      // If we find two cols the same, check it is a perfect reflection.
      if (getColumn(c) == getColumn(c + 1)) {
        foundVertical = true;
        var offset = 1;
        while (c - offset >= 0 && c + offset + 1 <= noCols - 1) {
          if (getColumn(c - offset) != getColumn(c + offset + 1)) {
            foundVertical = false;
            break;
          }
          offset++;
        }
      }
      if (foundVertical) {
        output.add(ReflectionLine(ReflectionType.vertical, c + 1));
      }
    }
    return output;
  }
}
