// Example 1: Factorial calculation
func factorial(n : int) : int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

func main() : int {
let result : int = factorial(5);
print(result);  // Deve imprimir 120
return 1;
}
