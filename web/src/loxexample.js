const originalLoxFileString = `
// To take into account time
var t1 = clock();

// Base class for a Calculator
class Calculator {
  compute(x) {
    return x; // Default implementation
  }
}

// Fibonacci calculator extends the base class
class Fibonacci < Calculator {
  compute(n) {
    if (n <= 1) return n;
    return this.compute(n - 1) + this.compute(n - 2);
  }
}

// Factorial calculator extends the base class
class Factorial < Calculator {
  compute(n) {
    if (n <= 1) return 1;
    return n * this.compute(n - 1);
  }
}

// Dynamic calculator handler
class DynamicCalculator {
  init(calculator) {
    this.calculator = calculator;
  }

  perform(input) {
    return this.calculator.compute(input);
  }
}

// Main execution
var input = 10;

fun showRes(name, func){
  print name + " of";
  print input;
  print "results in:";
  print func(input);
  print "";
}

// Using a Fibonacci calculator
var fibCalculator = DynamicCalculator(Fibonacci());
showRes("Fibonacci", fibCalculator.perform);

// Using a Factorial calculator
var factCalculator = DynamicCalculator(Factorial());
showRes("Factorial", factCalculator.perform);

// Total time
print "Total time (seconds)";
var t2 = clock();
print t2-t1;
`;

export { originalLoxFileString };
