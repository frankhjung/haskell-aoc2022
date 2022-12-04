# Advent of Code 2022 - Day 4

[Day 4 - Camp Cleanup](https://adventofcode.com/2022/day/n)

## Part 1

Space needs to be cleared before the last supplies can be unloaded from the
ships, and so several Elves have been assigned the job of cleaning up sections
of the camp. Every section has a unique ID number, and each Elf is assigned a
range of section IDs.

However, as some of the Elves compare their section assignments with each other,
they've noticed that many of the assignments overlap. To try to quickly find
overlaps and reduce duplicated effort, the Elves pair up and make a big list of
the section assignments for each pair (your puzzle input).

For example, consider the following list of section assignment pairs:

```text
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
```

For the first few pairs, this list means:

- Within the first pair of Elves, the first Elf was assigned sections 2-4
  (sections 2, 3, and 4), while the second Elf was assigned sections 6-8
  (sections 6, 7, 8).
- The Elves in the second pair were each assigned two sections.
- The Elves in the third pair were each assigned three sections: one got
  sections 5, 6, and 7, while the other also got 7, plus 8 and 9.
- This example list uses single-digit section IDs to make it easier to draw;
  your actual list might contain larger numbers. Visually, these pairs of
  section assignments look like this:

```text
.234.....  2-4
.....678.  6-8

.23......  2-3
...45....  4-5

....567..  5-7
......789  7-9

.2345678.  2-8
..34567..  3-7

.....6...  6-6
...456...  4-6

.23456...  2-6
...45678.  4-8
```

Some of the pairs have noticed that one of their assignments fully contains the
other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained by 4-6.
In pairs where one assignment fully contains the other, one Elf in the pair
would be exclusively cleaning sections their partner will already be cleaning,
so these seem like the most in need of reconsideration. In this example, there
are 2 such pairs.

In how many assignment pairs does one range fully contain the other?

## Part 2

## Notes

```haskell
λ> :m + Data.List.Split Data.Char Data.List Data.Attoparsec.Text Control.Monad Data.Text

λ> content <- readFile "test.data"
-- "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\n"

λ> r = parseInput (pack content)
Right [Record {range1 = (2,4), range2 = (6,8)},Record {range1 = (2,3), range2 = (4,5)},Record {range1 = (5,7), range2 = (7,9)},Record {range1 = (2,8), range2 = (3,7)},Record {range1 = (6,6), range2 = (4,6)},Record {range1 = (2,6), range2 = (4,8)}]

λ> Data.List.length $ fromRight [] r
6

λ> xs = lines content
-- ["2-4,6-8","2-3,4-5","5-7,7-9","2-8,3-7","6-6,4-6","2-6,4-8"]

λ> mylen = foldl' (\c t -> if t then succ c else c) 0

λ> as
[True,False,True]

-- count Trues
λ> mylen as
2
```

To check if range fully contained in another we have two scenarios:

```text
a <= c and d <= b

a  c  d  b
|  +--+  |
+--------+
```

And,

```text
c <= a and b <= d

c  a  b  d
|  +--+  |
+--------+
```

See also Data.Ix with it's `range` function:

```haskell
λ> :m Data.Ix
λ> :t range
range :: Ix a => (a, a) -> [a]

λ> range (2,10)
[2,3,4,5,6,7,8,9,10]
```

## References

* https://jakewheat.github.io/intro_to_parsing/#getting-started
* https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
* https://mmhaskell.com/blog/2018/2/26/attoparsec-the-clarity-of-do-syntax
* http://www.mchaver.com/posts/2016-05-09-attoparsec-tutorial-1.html
