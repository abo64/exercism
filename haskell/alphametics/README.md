# Alphametics

[Alphametics](https://en.wikipedia.org/wiki/Alphametics) is a puzzle where
letters in words are replaced with numbers.

For example `SEND + MORE = MONEY`:

```
  S E N D
  M O R E +
-----------
M O N E Y
```

Replacing these with valid numbers gives:

```
  9 5 6 7
  1 0 8 5 +
-----------
1 0 6 5 2
```

This is correct because every letter is replaced by a different number and the
words, translated into numbers, then make a valid sum.

Each letter must represent a different digit, and the leading digit of
a multi-digit number must not be zero.

Write a function to solve alphametics puzzles.


## Getting Started

For installation and learning resources, refer to the
[exercism help page](http://exercism.io/languages/haskell).

## Running the tests

To run the test suite, execute the following command:

```bash
stack test
```

#### If you get an error message like this...

```
No compiler found, expected minor version match with...
Try running "stack setup" to install the correct GHC...
```

Just do as it says and it will download and install
the correct compiler version:

```bash
stack setup
```

## Running *GHCi*

If you want to play with your solution in GHCi, just run the command:

```bash
stack ghci
```

## Feedback, Issues, Pull Requests

The [exercism/xhaskell](https://github.com/exercism/xhaskell) repository on
GitHub is the home for all of the Haskell exercises.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!



## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
