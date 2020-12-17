Problems_Description

Problem 1: Implementing the Luhn Algorithm (40 points total)
------------------------------------------------------------

The Luhn algorithm is used to check the validity of credit card numbers.
You can read about it on Wikipedia
[here](https://en.wikipedia.org/wiki/Luhn_algorithm). For this problem,
you will implement the Luhn algorithm in Haskell. The algorithm
encompasses the following steps:

1.  Double the value of every second digit beginning from the right.
    That is, the last digit is unchanged; the second-to-last digit is
    doubled; the third-to-last digit is unchanged; and so on. For
    example, `[1,3,8,6]`{.language-plaintext .highlighter-rouge} becomes
    `[2,3,16,6]`{.language-plaintext .highlighter-rouge}.
2.  Add the digits of the doubled values and the undoubled digits from
    the original number. For example, `[2,3,16,6]`{.language-plaintext
    .highlighter-rouge} becomes `2+3+1+6+6 = 18`{.language-plaintext
    .highlighter-rouge}.
3.  Calculate the remainder when the sum is divided by 10. For the above
    example, the remainder would be 8. If the result equals 0, then the
    number is valid.

### Problem 1.1 (8 points) {#problem-11-8-points}

We first need to be able to break up a number into its last digit and
the rest of the number. Write these functions:

``` {.highlight}
lastDigit :: Integer -> Integer
dropLastDigit :: Integer -> Integer
```

If you’re stumped, look through some of the arithmetic operators
mentioned in the lecture. You **may not** use lists or strings. This
should remind you of the `binary`{.language-plaintext
.highlighter-rouge} problem from the first homework…

Example output:

``` {.highlight}
GGHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude> :load HaskellIntro.hs 
[1 of 2] Compiling Set              ( Set.hs, interpreted )
[2 of 2] Compiling HaskellIntro     ( HaskellIntro.hs, interpreted )
Ok, two modules loaded.
*HaskellIntro> lastDigit 123
3
*HaskellIntro> lastDigit 0
0
*HaskellIntro> dropLastDigit 123
12
*HaskellIntro> dropLastDigit 5
0
*HaskellIntro> 
```

### Problem 1.2 (8 points) {#problem-12-8-points}

Now, we can break apart a number into its digits. Define the function

``` {.highlight}
toDigits :: Integer -> [Integer]
```

`toDigits`{.language-plaintext .highlighter-rouge} should convert
positive Integers to a list of digits. For `0`{.language-plaintext
.highlighter-rouge} or negative inputs, `toDigits`{.language-plaintext
.highlighter-rouge} should return the empty list.

Examples:

``` {.highlight}
toDigits 1234 == [1,2,3,4]
toDigits 0 == []
toDigits (-17) == []
```

### Problem 1.3 (8 points) {#problem-13-8-points}

Once we have the digits in the proper order, we need to double every
other one. Define a function

``` {.highlight}
doubleEveryOther :: [Integer] -> [Integer]
```

Remember that `doubleEveryOther`{.language-plaintext .highlighter-rouge}
should double every other number beginning from the right, that is, the
second-to-last, fourth-to-last, … numbers are doubled. Note that it’s
much easier to perform this operation on a list of digits that’s in
reverse order. You will likely need helper functions to make this work.

Examples:

``` {.highlight}
doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOther [1,2,3] == [1,4,3]
```

### Problem 1.4 (8 points) {#problem-14-8-points}

The output of `doubleEveryOther`{.language-plaintext .highlighter-rouge}
has a mix of one-digit and two-digit numbers. Define the function

``` {.highlight}
sumDigits :: [Integer] -> Integer
```

to calculate the sum of all digits.

Example:

``` {.highlight}
sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
```

### Problem 1.5 (8 points) {#problem-15-8-points}

Define the function

``` {.highlight}
validate :: Integer -> Bool
```

that indicates whether an `Integer`{.language-plaintext
.highlighter-rouge} could be a valid credit card number. This will use
all functions defined in the previous exercises.

Examples:

``` {.highlight}
validate 4012888888881881 = True
validate 4012888888881882 = False
```

Problem 2: Hofstadter Sequences (40 points)
-------------------------------------------

In this problem you will use a higher-order function to compute two
[Hofstadter
Sequences](https://en.wikipedia.org/wiki/Hofstadter_sequence), the
Hofstadter G sequence and the Hofstadter H sequence.

We will represent sequences not as lists, bus as functions that take an
index into the sequence and return the value of the sequence at that
index. Sequences will be indexed from 00

, so G(0)G(0)

will be the first item in the Hofstadter GG

sequence.

### Problem 2.1 (20 points) {#problem-21-20-points}

Write a higher-order function `pow f n`{.language-plaintext
.highlighter-rouge} that computes the function fnfn

, where fn(x)=(f∘⋯∘f)(x)fn(x)=(f∘⋯∘f)(x)

. That is, fnfn

is the function that composes the function ff

with itself nn

times. For example, given the following definition:

``` {.highlight}
square x = x*x
```

`pow square 2`{.language-plaintext .highlighter-rouge} would be the
function `square . square`{.language-plaintext .highlighter-rouge},
where `.`{.language-plaintext .highlighter-rouge} is the function
composition operator. Recall that we defined `.`{.language-plaintext
.highlighter-rouge} in class (it is also in the Prelude).

For any number nn

, n0=1n0=1

. Analogously, for any function ff

, f0f0

is the identity function.

Hint: Look at the type of `pow`{.language-plaintext .highlighter-rouge}.

### Problem 2.2 (10 points) {#problem-22-10-points}

Write functions `g`{.language-plaintext .highlighter-rouge} and
`h`{.language-plaintext .highlighter-rouge} that compute elements of the
Hofstadter G and Hofstadter H sequences, respectively. They are defined
as follows:

G(0)=0 G(n)=n−G(G(n−1))n\>0G(0)=0 G(n)=n−G(G(n−1))n\>0

H(0)=0 H(n)=n−H(H(H(n−1)))n\>0H(0)=0 H(n)=n−H(H(H(n−1)))n\>0

You **must** define `g`{.language-plaintext .highlighter-rouge} and
`h`{.language-plaintext .highlighter-rouge} such that **all** recursive
calls to `g`{.language-plaintext .highlighter-rouge} and
`h`{.language-plaintext .highlighter-rouge} occur indirectly via
`pow`{.language-plaintext .highlighter-rouge}, i.e., you may not
recursively call `g`{.language-plaintext .highlighter-rouge} or
`h`{.language-plaintext .highlighter-rouge} directly or through a helper
function that is not `pow`{.language-plaintext .highlighter-rouge}.

The first few numbers in the GG

sequence are
0,1,1,2,3,3,4,4,5,6,6,7,8,8,9,9,10,11,11,12,120,1,1,2,3,3,4,4,5,6,6,7,8,8,9,9,10,11,11,12,12

.

The first few numbers in the HH

sequence are
0,1,1,2,3,4,4,5,5,6,7,7,8,9,10,10,11,12,13,13,140,1,1,2,3,4,4,5,5,6,7,7,8,9,10,10,11,12,13,13,14

.

### Problem 2.3 (10 points) {#problem-23-10-points}

The G and H sequences can be generalized to a family of sequences, DiDi

:

Di(0)=0 Di(n)=n−Dii(n−1)n\>0Di(0)=0 Di(n)=n−Dii(n−1)n\>0

Observe that G=D2G=D2

and H=D3H=D3

.

Write a function `d i`{.language-plaintext .highlighter-rouge} that
computes elements of the DiDi

sequence. Note that DiiDii

is just Di∘…∘Dii instances of DiDi∘…∘Di⏟i instances
of Di

.

You **must** define `d`{.language-plaintext .highlighter-rouge} using
`pow`{.language-plaintext .highlighter-rouge}.

The first few numbers in the D4D4

sequence are
0,1,1,2,3,4,5,5,6,6,7,8,8,9,10,11,11,12,13,14,150,1,1,2,3,4,5,5,6,6,7,8,8,9,10,11,11,12,13,14,15

.

Hint: The function `d`{.language-plaintext .highlighter-rouge} can be
partially applied to give a function that compute the elements of a
particular sequence DiDi

. For example, the Haskell functions `d 2`{.language-plaintext
.highlighter-rouge} and `g`{.language-plaintext .highlighter-rouge}
should be equivalent at every non-negative argument.

Problem 3: Power set of a set (20 points)
-----------------------------------------

Implement the power set function, P(⋅)P(⋅)

, from lecture. Recall that the power set P(S)P(S)

of a set SS

is the set of all subsets of SS

, including the empty set and SS

itself. Also recall the inductive proof that a set with nn

elements has a power set with 2n2n

elements.

The type signature for `powerSet`{.language-plaintext
.highlighter-rouge} should look like this:

``` {.highlight}
powerSet :: ... => ... -> ...
```

Before you write the function, you should figure out what its type
signature should be. If you want to start by specializing the function
to only work with sets of a particular type, like
`Int`{.language-plaintext .highlighter-rouge}, that’s OK, but your final
type signature should be polymorphic.

One of the goals of this problem is to treat `Set`{.language-plaintext
.highlighter-rouge} as an abstract data type; we should be able to
change the representation of sets, keeping the external interface the
same, and the code you wrote to compute the power set would run
*unchanged*. This means that you cannot rely in any way on the fact that
sets happen to be implemented as sorted lists with no duplicates. How
then can you test for an empty set? Fortunately, there is an
`isEmpty`{.language-plaintext .highlighter-rouge} predicate exported by
the `Set`{.language-plaintext .highlighter-rouge} module. How can you
split a set into an element plus the rest of the set? Again, we
fortunately have a `split`{.language-plaintext .highlighter-rouge}
function exported by the `Set`{.language-plaintext .highlighter-rouge}
module.

If you are stuck trying to pattern match on the structure of a set,
stop! You can’t do that, since you don’t get to see the representation!
Instead, pattern match and use guards, or, if you prefer, just use
Haskell’s `if`{.language-plaintext .highlighter-rouge}.

Please note the following constraints:

1.  You **may not** change the code in `Set.hs`{.language-plaintext
    .highlighter-rouge}.
2.  Solutions that themselves use `fromList`{.language-plaintext
    .highlighter-rouge} or `toList`{.language-plaintext
    .highlighter-rouge} will receive zero credit. You must use the
    `Set`{.language-plaintext .highlighter-rouge} abstraction without
    relying on the fact that “under the covers” it is implemented using
    lists. However, you **may** use `fromList`{.language-plaintext
    .highlighter-rouge} and `toList`{.language-plaintext
    .highlighter-rouge} to test your code!
3.  You may use any function that is exported by the
    `Set`{.language-plaintext .highlighter-rouge} module **except
    `fromList`{.language-plaintext .highlighter-rouge} or
    `toList`{.language-plaintext .highlighter-rouge}**, even if it uses
    `fromList`{.language-plaintext .highlighter-rouge} or
    `toList`{.language-plaintext .highlighter-rouge} internally. Think
    of the `Set`{.language-plaintext .highlighter-rouge} module as a
    black box whose implementation you are not allowed to see.
4.  You may not write your own functions that convert between
    `Set`{.language-plaintext .highlighter-rouge}s and lists in order to
    bypass the restriction on using `fromList`{.language-plaintext
    .highlighter-rouge} and `toList`{.language-plaintext
    .highlighter-rouge}.

In essence **use sets, not lists** to solve this problem.

