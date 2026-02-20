// Example 6: Generic map function
forall a b . func map(f : (a) -> b, v : a[]) : b[] {
    let result = new b[v.size];
    for (i = 0; i < v.size; i + 1) {
        result[i] = f(v[i]);
    }
    return result;
}
