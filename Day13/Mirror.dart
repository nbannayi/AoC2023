import 'ReflectionLine.dart';

// Mirror object.
class Mirror {
  List<String> rows;

  Mirror(String rawText) : rows = rawText.split('\n');

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

  // Get reflection line in this mirror.
  List<ReflectionLine> getReflectionLines() {
    var noRows = rows.length;
    List<ReflectionLine> reflectionLines = [];

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
      // If found exit.
      if (foundHorizontal) {
        reflectionLines
            .add(new ReflectionLine(ReflectionType.horizonal, r + 1));
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
      // If found exit.
      if (foundVertical) {
        reflectionLines.add(new ReflectionLine(ReflectionType.vertical, c + 1));
      }
    }

    return reflectionLines;
  }
}
