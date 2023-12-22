// Type of reflection line.
enum ReflectionType { horizonal, vertical }

// Reflection line object.
class ReflectionLine {
  ReflectionType reflectionType;
  int position;

  ReflectionLine(this.reflectionType, this.position);

  // Summarise the reflection line for notes.
  int summarise() {
    if (this.reflectionType == ReflectionType.horizonal) {
      return 100 * position;
    } else {
      return position;
    }
  }

  // Display the reflection line.
  void display() {
    var summary = summarise();
    print('$reflectionType: $position, summary: $summary');
  }
}
