# Advent of Code 2022 - Day 8

[Day 8 - Treetop Tree House](https://adventofcode.com/2022/day/8)

## Part 1

The expedition comes across a peculiar patch of tall trees all planted carefully
in a grid. The Elves explain that a previous expedition planted these trees as a
reforestation effort. Now, they're curious if this would be a good location for
a tree house.

First, determine whether there is enough tree cover here to keep a tree house
hidden. To do this, you need to count the number of trees that are visible from
outside the grid when looking directly along a row or column.

The Elves have already launched a quadcopter to generate a map with the height
of each tree (your puzzle input). For example:

```text
30373
25512
65332
33549
35390
```

Each tree is represented as a single digit whose value is its height, where 0 is
the shortest and 9 is the tallest.

A tree is visible if all of the other trees between it and an edge of the grid
are shorter than it. Only consider trees in the same row or column; that is,
only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are visible - since they are
already on the edge, there are no trees to block the view. In this example, that
only leaves the interior nine trees to consider:

- The top-left 5 is visible from the left and top. (It isn't visible from the
  right or bottom since other trees of height 5 are in the way.)
- The top-middle 5 is visible from the top and right.
- The top-right 1 is not visible from any direction; for it to be visible, there
  would need to only be trees of height 0 between it and an edge.
- The left-middle 5 is visible, but only from the right.
- The centre 3 is not visible from any direction; for it to be visible, there
  would need to be only trees of at most height 2 between it and an edge.
- The right-middle 3 is visible from the right.
- In the bottom row, the middle 5 is visible, but the 3 and 4 are not.

With 16 trees visible on the edge and another 5 visible in the interior, a total
of 21 trees are visible in this arrangement.

Consider your map; how many trees are visible from outside the grid?

## Part 2

## Notes

If visible (left or right or top or bottom) for each inner item (not perimeter)
then sum of visible & perimeter gives solution:

> How many trees are visible from outside the grid?

```text
Outline of method:

- take row or column
- consider inner entries only (not first or last)
- for each inner entry
    - is this strictly bigger than
        left entries?
        right entries?
        top entries?
        bottom entries?
    - if any answer to this is yes, then mark entry as visible
```

Visible on the edge is `5 * length - 4 (one for each corner) = 16`

```haskell

$ stack repl
λ> :set -package array-0.5.4.0
λ> :m + Data.List Data.Ix

λ> contents <- readFile "test.data"
"30373\n25512\n65332\n33549\n35390\n"

λ> rs = parse contents
[3,0,3,7,3,2,5,5,1,2,6,5,3,3,2,3,3,5,4,9,3,5,3,9,0]

rs = [3,0,3,7,3,2,5,5,1,2,6,5,3,3,2,3,3,5,4,9,3,5,3,9,0]

-- list Array needs a single list of integers
λ> as = listArray ((0,0),(4,4)) (concat rs)
array ((0,0),(4,4)) [((0,0),3),((0,1),0),((0,2),3),((0,3),7),((0,4),3),((1,0),2),((1,1),5),((1,2),5),((1,3),1),((1,4),2),((2,0),6),((2,1),5),((2,2),3),((2,3),3),((2,4),2),((3,0),3),((3,1),3),((3,2),5),((3,3),4),((3,4),9),((4,0),3),((4,1),5),((4,2),3),((4,3),9),((4,4),0)]

-- get by index (0 based indices)
λ> as ! (1,1)
5
```
Is character at `(r,c)` visible from the left:
- check that all values from index 0 to `c - 1` are `< as ! (r,c)`
