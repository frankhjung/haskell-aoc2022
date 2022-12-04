# Advent of Code 2022 - Day 2

[Day 2 - Rock Paper Scissors](https://adventofcode.com/2022/day/2)

## Part 1

What would your total score be if everything goes exactly according to your
strategy guide?

Given:

### Codes

  {A,B,C} = {Rock, Paper, Scissors}
  {X,Y,Z} = {Rock, Paper, Scissors}

### Play Score

  1 - Rock
  2 - Paper
  3 - Scissors

### Win Score

  0 - Lose
  3 - Draw
  6 - Win

### Example

  ----+----------+----------+------
  Code| Player A | Player B | Score
  ----+----------+----------+------
  A Y | Rock     | Paper    | 2 + 6
  B X | Paper    | Rock     | 1 + 0
  C Z | Scissors | Scissors | 3 + 3
  ----+----------+----------+------

Final tally:

  8 + 1 + 6 = 15

## Part 2

> Anyway, the second column says how the round needs to end: X means you need
> to lose, Y means you need to end the round in a draw, and Z means you need to
> win. Good luck!


### Example

  {X,Y,Z} = {Lose, Draw, Win}

  ----+----------+----------+----------+------
  Code| Player A | Result   | Player B | Score
  ----+----------+----------+----------+------
  A Y | Rock     | Draw     | Rock     + 1 + 3
  B X | Paper    | Lose     | Rock     + 1 + 0
  C Z | Scissors | Win      | Rock     + 1 + 6
  ----+----------+----------+----------+------

Final tally:

  4 + 1 + 7 = 12

