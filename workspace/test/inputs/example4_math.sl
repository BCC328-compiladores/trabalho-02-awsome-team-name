// Example 4: Mathematical and Boolean Operations
func calculateBMI(weight : float, height : float) : float {
    return weight / (height * height);
}

func isAdult(age : int) : bool {
    return age >= 18;
}

func main() : void {
    let bmi : float = calculateBMI(70.5, 1.75);
    let adult : bool = isAdult(20);
    print(bmi);
    print(adult);
    if (adult && bmi > 25.0) {
        print("Adulto com sobrepeso");
    } else {
        print("Condicao normal");
    }
}
