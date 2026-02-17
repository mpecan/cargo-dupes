pub fn process_data(input: Vec<i32>) -> i32 {
    let mut sum = 0;
    for item in input.iter() {
        if *item > 0 {
            sum += *item;
        }
    }
    sum
}

pub fn compute_total(values: Vec<i32>) -> i32 {
    let mut sum = 0;
    for item in values.iter() {
        if *item > 0 {
            sum += *item;
        }
    }
    sum
}

pub fn aggregate(numbers: Vec<i32>) -> i32 {
    let mut sum = 0;
    for item in numbers.iter() {
        if *item > 0 {
            sum += *item;
        }
    }
    sum
}
