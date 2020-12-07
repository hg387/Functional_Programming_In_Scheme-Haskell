Problems Descriptions:

Problem 1: `(sigma m n)`{.language-plaintext .highlighter-rouge} (20 points)
----------------------------------------------------------------------------

Define a **pure**, **recursive** function `sigma`{.language-plaintext
.highlighter-rouge} that takes two integer arguments, mm

and nn

, and returns the sum of the numbers mm

through nn

, inclusive, i.e., ∑ni=mi∑i=mni

. You may assume m≤nm≤n

.

**My solution: 3 lines**

Problem 2: `(log m n)`{.language-plaintext .highlighter-rouge} (20 points total)
--------------------------------------------------------------------------------

Define a **pure**, **recursive** function `log`{.language-plaintext
.highlighter-rouge} that takes two arguments, mm

and nn

, and returns the least integer ll

such that ml+1\>nml+1\>n

. You may assume mm

is a positive integer and n≥1n≥1

.

Your solution **may not** calculate an exponent using either a library
function or a function you have written.

**My solution: 3 lines**

Problem 3: `(choose n k)`{.language-plaintext .highlighter-rouge} (20 points total)
-----------------------------------------------------------------------------------

Define a **pure**, **recursive** function `choose`{.language-plaintext
.highlighter-rouge} that takes two arguments, nn

and kk

, and calculates (nk)(nk)

, the binomial coefficient, which is also the number of ways of
selecting kk

items from a collection of nn

items. You may assume that nn

and kk

are both non-negative integers.

Your `choose`{.language-plaintext .highlighter-rouge} should call itself
recursively. An implementation that uses the closed-form formula for
`choose`{.language-plaintext .highlighter-rouge} involving factorial
will receive zero credit.

If you have forgotten the definition of the binomial coefficient, you
may want to read the [Wikipedia
article](https://en.wikipedia.org/wiki/Binomial_coefficient). The
following recurrence relation will be useful:

(nk)=(n−1k−1)+(n−1k)(nk)=(n−1k−1)+(n−1k)

Make sure you handle the base cases:

(n0)=(nn)=1(n0)=(nn)=1

**My solution: 6 lines (I did some extra error checking)**

Problem 4: `(binary n)`{.language-plaintext .highlighter-rouge} (20 points)
---------------------------------------------------------------------------

Define a **pure**, **recursive** function `binary`{.language-plaintext
.highlighter-rouge} that takes a single argument nn

and returns the number whose decimal representation is the binary
representation nn

. For example, `(binary 2)`{.language-plaintext .highlighter-rouge}
should evaluate to `10`{.language-plaintext .highlighter-rouge}. You may
assume that nn

is a non-negative integer.

Your solution **may not** use strings, Racket’s binary number type, or
lists. Your function must return a **decimal** number.

**My solution: 3 lines**

Problem 5: `(scan f z l)`{.language-plaintext .highlighter-rouge} (20 points)
-----------------------------------------------------------------------------

Define a **pure**, **recursive** function `scan`{.language-plaintext
.highlighter-rouge} that takes a binary function `f`{.language-plaintext
.highlighter-rouge}, a value `z`{.language-plaintext
.highlighter-rouge}, and a list `l`{.language-plaintext
.highlighter-rouge}, and returns the list
z,f(z,x1),f(f(z,x1),x2),…,f(f(…f(f(z,x1),x2)…,xn−1),xn)z,f(z,x1),f(f(z,x1),x2),…,f(f(…f(f(z,x1),x2)…,xn−1),xn)

where x1,x2,…,xnx1,x2,…,xn

are the elements of the list `l`{.language-plaintext
.highlighter-rouge}.

Examples:

``` {.highlight}
(scan + 0 null) => (0)
(scan + 0 '(1 2 3 4 5 6)) => (0 1 3 6 10 15 21)
(scan * 1 '(1 2 3 4 5 6)) => (1 1 2 6 24 120 720)
```

This may remind you of the `foldl`{.language-plaintext
.highlighter-rouge} function. You can think of
`scan`{.language-plaintext .highlighter-rouge} as a version of
`foldl`{.language-plaintext .highlighter-rouge} that returns a list of
all the intermediate results of the fold.

Hint: Try first writing a function that returns all the intermediate
sums of a list. That is, assume `f`{.language-plaintext
.highlighter-rouge} is `+`{.language-plaintext .highlighter-rouge} and
`z`{.language-plaintext .highlighter-rouge} is `0`{.language-plaintext
.highlighter-rouge}. Then try to generalize your function.

**My solution: 3 lines**

Problem 6: Homework Statistics (1 point)
----------------------------------------

How long did spend on each problem? Please tell us in your
`README.md`{.language-plaintext .highlighter-rouge}.

You may enter time using any format described
[here](https://github.com/wroberts/pytimeparse). Please enter times
using the requested format, as that will make it easy for us to
automatically collect.

We are happy to read additional comments you have, but **do not** put
any comments on the same line as the time report. Please **do not**
otherwise edit, move, or reformat the lines reserved for time statistics
reports.

Here is the reporting format you should use:

``` {.highlight}
Problem 1: 5m

Here's a description of my experience solving this problem.
```

If you did not attempt one or more problems, you can still receive full
marks by telling us you spent 0 minutes on these problems. You will not
receive points for this problem if you leave **any** of the time reports
blank.

Copyright © Geoffrey Mainland/Drexel University 2015–2021
