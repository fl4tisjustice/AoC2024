use std::fs::read_to_string;
use regex::Regex;

fn part_one(data: &str) -> u64 {
    let re = Regex::new(r"mul\((?<op1>[0-9]{1,3}),(?<op2>[0-9]{1,3})\)").unwrap();
    re.captures_iter(data)
        .map(|capture| capture.extract())
        .map(|(_, [op1, op2])| op1.parse::<u64>().unwrap() * op2.parse::<u64>().unwrap()).sum()
}

fn part_two(data: &str) -> u64 {
    let re = Regex::new(r"(?ms)don't\(\).*?do\(\)").unwrap();
    part_one(&re.replace_all(data, ""))
}

fn main() {
    let data = read_to_string("day-3/input.txt").unwrap();
    println!("Part One: {}\nPart Two: {}", part_one(&data), part_two(&data));
}
