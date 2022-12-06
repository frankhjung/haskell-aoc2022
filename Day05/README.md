# Advent of Code 2022 - Day 5

[Day 5 - Supply Stacks](https://adventofcode.com/2022/day/5)

## Part 1

The expedition can depart as soon as the final supplies have been unloaded from
the ships. Supplies are stored in stacks of marked crates, but because the
needed supplies are buried under many other crates, the crates need to be
rearranged.

The ship has a giant cargo crane capable of moving crates between stacks. To
ensure none of the crates get crushed or fall over, the crane operator will
rearrange them in a series of carefully planned steps. After the crates are
rearranged, the desired crates will be at the top of each stack.

The Elves don't want to interrupt the crane operator during this delicate
procedure, but they forgot to ask her which crate will end up where, and they
want to be ready to unload them as soon as possible so they can embark.

They do, however, have a drawing of the starting stacks of crates and the
rearrangement procedure (your puzzle input). For example:

```text
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3
```

```text
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
```

In this example, there are three stacks of crates. Stack 1 contains two crates:
crate `Z` is on the bottom, and crate `N` is on top. Stack 2 contains three
crates; from bottom to top, they are crates `M`, `C`, and `D`. Finally, stack 3
contains a single crate, `P`.

Then, the rearrangement procedure is given. In each step of the procedure, a
quantity of crates is moved from one stack to a different stack. In the first
step of the above rearrangement procedure, one crate is moved from stack 2 to
stack 1, resulting in this configuration:

```text
[D]
[N] [C]
[Z] [M] [P]
 1   2   3
```

In the second step, three crates are moved from stack 1 to stack 3. Crates are
moved one at a time, so the first crate to be moved (`D`) ends up below the
second and third crates:

```text
        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3
```

Then, both crates are moved from stack 2 to stack 1. Again, because crates are
moved one at a time, crate `C` ends up below crate `M`:

```text
        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3
```

Finally, one crate is moved from stack 1 to stack 2:

```text
        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3
```

The Elves just need to know which crate will end up on top of each stack; in
this example, the top crates are `C` in stack 1, `M` in stack 2, and `Z` in
stack 3, so you should combine these together and give the Elves the message
`CMZ`.

After the rearrangement procedure completes, what crate ends up on top of each
stack?

## Part 2

As you watch the crane operator expertly rearrange the crates, you notice the
process isn't following your prediction.

Some mud was covering the writing on the side of the crane, and you quickly wipe
it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.

The CrateMover 9001 is notable for many new and exciting features: air
conditioning, leather seats, an extra cup holder, and the ability to pick up and
move multiple crates at once.

In this example, the CrateMover 9001 has put the crates in a totally different
order: `MCD`.

## Notes

Parse input as fixed width strings:

- ignore id of stack (we will re-create this)

```haskell
λ> :m +Solve Data.List.Split Data.List Data.Char Data.Either Control.Lens Data.Array.Unboxed

λ> contents <- readFile "test.data"
λ> (stacks,moves) = parse contents

-- total number of crates
λ> length . concat $ stacks
6

-- number of stacks (see size below) = 3
s = listArray (1,3) $ zip [1..] stacks

λ> splitWhen (=="") (lines contents)
[["    [D]","[N] [C]","[Z] [M] [P]"," 1   2   3"],["move 1 from 2 to 1","move 3 from 1 to 3","move 2 from 2 to 1","move 1 from 1 to 2"]]

λ> [crates',moves] = splitWhen (=="") (lines contents)

-- crates' + last row containing stack id
λ> crates'
["    [D]","[N] [C]","[Z] [M] [P]"," 1   2   3"]

-- number of stacks containing crates
λ> size = ((read . last . words . last) crates') :: Int
3

-- use attoparsec to parse moves to data structure
λ> moves
["move 1 from 2 to 1","move 3 from 1 to 3","move 2 from 2 to 1","move 1 from 1 to 2"]

λ> crates = take size $ crates'
["    [D]","[N] [C]","[Z] [M] [P]"]

λ> mapM_ print crates
"    [D]"
"[N] [C]"
"[Z] [M] [P]"
```

Notice the fixed positions of the crates in a stack:

```text
 1   5   9
    [D]
[N] [C]
[Z] [M] [P]   -- letters appear in positions 1,5,9,13
```

Select every _n_ th item from the list:

```haskell
λ> every n = map snd . filter ((== 1) . fst) . zip (cycle [1..n])
```

Select every 4th item (after dropping first column
The result is a list where the position of each item is the stack it belongs
too. i.e. `D` belongs to stack 2:

```haskell
λ> rows = map (\c -> every 4 (drop 1 c)) crates
[" D","NC","ZMP"]
```

To build the stack from this input:

```haskell
λ> mkStack = concatMap (filter (not . isSpace) . take 1)
λ> mkStack rows
"NZ"

λ> dropStack = map (drop 1)
λ> dropStack rows
["D","C","MP"]
```

Then iterate until rows is empty.

Command sequence looks like:

```haskell
λ> contents <- readFile "test.data"
λ> crates = take 3 $ lines contents
λ> rows = map (\c -> every 4 (drop 1 c)) crates
[" D","NC","ZMP"]
λ> mkStack rows
["NZ","DCM","P"]
```

This gives us our 3 stacks on which we then can apply the move actions.

Parse moves as a series of actions.

```text
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
```

Generically:

  move _n_ from _s_ to _t_

Means:

  take head from s and prepend to _t_
  do this n times

### Data.Array

Started with:

```bash
stack repl
```

GHCi session:

```haskell
λ> :m + Data.Array Data.Ix

λ> contents <- readFile "test.data"
λ> (stacks,moves) = parse contents

λ> s = listArray (1,3) $ zip [1..] stacks
λ> print s
array (1,3) [(1,(1,"NZ")),(2,(2,"DCM")),(3,(3,"P"))]

λ> s!1
(1,"NZ")

λ> s!2
(2,"DCM")
λ> snd $ s!2
"DCM"

-- so moving 2 from 1 to 2 looks like:

λ> foldr (:) "DCM" (reverse (take 2 "NZ"))
"ZNDCM"

-- and moving 1 from 1 to 2 looks like:

λ> foldr (:) "DCM" (reverse (take 1 "NZ"))
"NDCM"

-- now replace existing array with updated entries
λ> s' = s // [(1,(1,"ZNDCM")),(2,(2,""))]
array (1,3) [(1,(1,"ZNDCM")),(2,(2,"")),(3,(3,"P"))]

λ> s'!1
(1,"ZNDCM")

λ> s'!2
(2,"")

```

Compare this to `accumArray`:

```haskell
*Main Solve Data.Array Data.List Data.Char
λ> s
array (1,3) [(1,"NZ"),(2,"DCM"),(3,"P")]

λ> accumArray (flip (++)) (s!1) (1,1) [(1, reverse $ take 3 (s!2))]
array (1,1) [(1,"MCDNZ")]
```

### Puzzle input:

```text
0123456789012345678901234567890123456789
 1   5   9  13  17  21  25  29  33  -- i.e. every 4th position from 1
            [G] [W]         [Q]
[Z]         [Q] [M]     [J] [F]
[V]         [V] [S] [F] [N] [R]
[T]         [F] [C] [H] [F] [W] [P]
[B] [L]     [L] [J] [C] [V] [D] [V]
[J] [V] [F] [N] [T] [T] [C] [Z] [W]
[G] [R] [Q] [H] [Q] [W] [Z] [G] [B]
[R] [J] [S] [Z] [R] [S] [D] [L] [J]
 1   2   3   4   5   6   7   8   9
```
