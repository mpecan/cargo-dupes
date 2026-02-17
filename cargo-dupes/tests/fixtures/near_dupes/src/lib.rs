pub fn process_positive(input: Vec<i32>) -> i32 {
    let mut sum = 0;
    for item in input.iter() {
        if *item > 0 {
            sum += *item;
        }
    }
    sum
}

pub fn process_negative(input: Vec<i32>) -> i32 {
    let mut sum = 0;
    for item in input.iter() {
        if *item < 0 {
            sum += *item;
        }
    }
    sum
}

pub fn process_even(input: Vec<i32>) -> i32 {
    let mut sum = 0;
    for item in input.iter() {
        if *item % 2 == 0 {
            sum += *item;
        }
    }
    sum
}
