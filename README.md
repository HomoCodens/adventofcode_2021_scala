# Advent of Code 2021 in Scala

Solutions for [Advent of Code 2021](https://adventofcode.com/2021)

## Personal goals

* Get to know a new language
* See how good I can get in a month
* Save christmas!

## Take aways

While some things seemed fishy at first (`def foo(f: Int)(x: Int)`) the language has really grown on me. The pattern matching and `apply`/`unapply` could well be my favourite features learned in 2021.

## Requirements
* Scala `2.13.7`
* sbt `1.5.5`

## Setup
```
git clone https://github.com/HomoCodens/adventofcode_2021_scala
cd adventofcode_2021_scala
```
Optionally:
Copy your own inputs into `inputs/dayX/input.txt`. Only example inputs (`-t <x>`) are included in this repo.
```
sbt
run -d 1 -i ./inputs -t 1
run --help
```

## Usage
```
  -t, --test <test case>  Run day against test input.
  -v, --verbose           Print addidional output.
  -T, --time              Time the solution.
  -d, --day <day>         The day to run.
  -i, --input <input>     Path to input folder.
  --help                  Print this help message.
```