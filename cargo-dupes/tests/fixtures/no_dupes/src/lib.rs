pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

pub fn factorial(n: u64) -> u64 {
    if n <= 1 {
        return 1;
    }
    let mut result = 1u64;
    let mut i = 2;
    while i <= n {
        result *= i;
        i += 1;
    }
    result
}
