// Functions with identical if-then branches across different functions.
// The functions themselves differ, but their if-then branches are structurally identical.

pub fn handle_positive(x: i32) -> i32 {
    if x > 0 {
        let doubled = x * 2;
        let incremented = doubled + 1;
        let result = incremented * incremented;
        result
    } else {
        x
    }
}

pub fn process_value(val: i32) -> i32 {
    if val > 0 {
        let doubled = val * 2;
        let incremented = doubled + 1;
        let result = incremented * incremented;
        result
    } else {
        val - 1
    }
}

// Functions with identical match arm bodies across different functions.

pub fn classify_number(n: i32) -> &'static str {
    match n {
        0 => {
            let _zero_flag = true;
            let _msg = "nothing";
            "zero"
        }
        x if x > 0 => {
            let abs = x;
            let doubled = abs * 2;
            let _result = doubled + abs;
            "positive"
        }
        _ => {
            let abs = -n;
            let doubled = abs * 2;
            let _result = doubled + abs;
            "negative"
        }
    }
}

pub fn describe_value(v: i32) -> &'static str {
    match v {
        0 => "none",
        x if x > 0 => {
            let abs = x;
            let doubled = abs * 2;
            let _result = doubled + abs;
            "big"
        }
        _ => "small",
    }
}

// Functions with identical loop bodies.

pub fn sum_doubled(items: &[i32]) -> i32 {
    let mut total = 0;
    for item in items.iter() {
        let doubled = item * 2;
        let adjusted = doubled + 1;
        total += adjusted;
    }
    total
}

pub fn accumulate(values: &[i32]) -> i32 {
    let mut total = 0;
    for value in values.iter() {
        let doubled = value * 2;
        let adjusted = doubled + 1;
        total += adjusted;
    }
    total
}
