# [Advent of Code 2022](https://adventofcode.com/2022/)

Copyright © 2022 Frank H Jung

* [Day 1 - Calorie Counting](Day01/README.md)
* [Day 2 - Rock Paper Scissors](Day02/README.md)
* [Day 3 - Rucksack Reorganisation](Day03/README.md)
* [Day 4 - Camp Cleanup](Day04/README.md)
* [Day 5 - Supply Stacks](Day05/README.md)
* [Day 6 - Tuning Trouble](Day06/README.md)
* [Day 7 - No Space Left On Device](Day07/README.md)
* [Day 8 - Treetop Tree House](Day08/README.md)
* [Day 9 - Rope Bridge](Day09/README.md)
* [Day 10 - Cathode-Ray Tube](Day10/README.md)
* [Day 11 - Monkey in the Middle](Day11/README.md)

## Create Advent of Code Day from Template

```bash
stack new Day01 template/day.hsfiles
```

Then, build the project to check:

```bash
cd Day01
make
```

As a single command:

```bash
day=DayNN; stack new ${day} template/day.hsfiles; cd ${day}; make
```

The default make target will perform: `check`, `build`, `test` and `exec`.

## Hints

To clean up all directories:

```bash
for f in $(find . -maxdepth 1 -name "Day*" -type d); do make -C $f clean ; done
```

Other build targets are:

* `clean`    - clean generated files
* `cleanall` - purge all generated files
