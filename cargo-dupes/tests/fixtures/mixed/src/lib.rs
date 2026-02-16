// Exact duplicates
pub fn sum_positive(data: Vec<i32>) -> i32 {
    let mut total = 0;
    for val in data.iter() {
        if *val > 0 {
            total += *val;
        }
    }
    total
}

pub fn count_positive(values: Vec<i32>) -> i32 {
    let mut total = 0;
    for val in values.iter() {
        if *val > 0 {
            total += *val;
        }
    }
    total
}

// Near duplicates (different comparison operator)
pub fn sum_negative(data: Vec<i32>) -> i32 {
    let mut total = 0;
    for val in data.iter() {
        if *val < 0 {
            total += *val;
        }
    }
    total
}

// Unique function
pub fn fibonacci(n: u32) -> u32 {
    if n <= 1 {
        return n;
    }
    let mut a = 0u32;
    let mut b = 1u32;
    let mut i = 2;
    while i <= n {
        let temp = a + b;
        a = b;
        b = temp;
        i += 1;
    }
    b
}
