Problems_Description

Problem 1: `(stream-scan f z s)`{.language-plaintext .highlighter-rouge} (20 points)
------------------------------------------------------------------------------------

Modify your `scan`{.language-plaintext .highlighter-rouge} function from
Hoemwork 1 so that it takes a stream as an argument and returns a stream
as the result—`stream-scan`{.language-plaintext .highlighter-rouge} must
not use lists. Your solution must be **pure** and **recursive**.

**My solution: 3 lines**

Problem 2: `(stream-take-n n s)`{.language-plaintext .highlighter-rouge} (20 points)
------------------------------------------------------------------------------------

Write a **pure**, **recursive** function
`stream-take-n`{.language-plaintext .highlighter-rouge} that takes two
arguments, an integer `n`{.language-plaintext .highlighter-rouge} and a
stream `s`{.language-plaintext .highlighter-rouge}, and returns a list
consisting of the first `n`{.language-plaintext .highlighter-rouge}
items in the stream.

**My solution: 3 lines**

Problem 3: `(stream-pair-with f s)`{.language-plaintext .highlighter-rouge} (20 points)
---------------------------------------------------------------------------------------

Write a **pure** function `stream-pair-with`{.language-plaintext
.highlighter-rouge} that takes a function `f`{.language-plaintext
.highlighter-rouge} and a stream `s`{.language-plaintext
.highlighter-rouge}, consisting of elements x1,x2,…x1,x2,…

, and returns a new stream where each element xixi

of `s`{.language-plaintext .highlighter-rouge} has been paired with
fxifxi

.

Examples:

``` {.highlight}
(stream->list (stream-pair-with (lambda (x) (+ x 1)) (stream 1 2 3 4))) => ((1 . 2) (2 . 3) (3 . 4) (4 . 5))
```

**My solution: 1 line**

Problem 4: `(cycle-streams xs ys)`{.language-plaintext .highlighter-rouge} (20 points)
--------------------------------------------------------------------------------------

Write a **pure** function `cycle-streams`{.language-plaintext
.highlighter-rouge} that takes streams, `xs`{.language-plaintext
.highlighter-rouge} and `ys`{.language-plaintext .highlighter-rouge},
and returns a stream. The streams may or may not be the same length, and
either one may be infinite, but you may assume they are both non-empty.
The elements produced by the stream are pairs where the first part is
from `xs`{.language-plaintext .highlighter-rouge} and the second part is
from `ys`{.language-plaintext .highlighter-rouge}. The stream returned
by `cycle-streams`{.language-plaintext .highlighter-rouge} cycles
forever through the streams `xs`{.language-plaintext .highlighter-rouge}
and `ys`{.language-plaintext .highlighter-rouge}. A constant-time
(O(1)O(1)

) solution is possible.

Examples:

``` {.highlight}
(stream-take-n 8 (cycle-streams (stream 1 2 3) (stream "a" "b"))) => ((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a") (2 . "b"))
```

Hint:

1.  Write a function that creates a stream that endlessly cycles through
    all the elements of a single, given stream. You can use a local
    environment to store a reference to the original stream through
    which you must cycle and a helper function that uses this stored
    reference when it runs out of elements to cycle through.
2.  Write a function that pairs up all the elements in two (possibly
    infinite) streams.
3.  Compose the two functions you just wrote to implement
    `cycle-streams`{.language-plaintext .highlighter-rouge}.

**My solution: 10 lines**

Problem 5: `(seen x)`{.language-plaintext .highlighter-rouge} (20 points)
-------------------------------------------------------------------------

Write a function `seen`{.language-plaintext .highlighter-rouge} that
takes a single argument `x`{.language-plaintext .highlighter-rouge} and
return `#t`{.language-plaintext .highlighter-rouge} if it has been
previously called with “the same” argument and `#f`{.language-plaintext
.highlighter-rouge} otherwise. That is, `seen`{.language-plaintext
.highlighter-rouge} should remember every argument it has ever seen.

Two values are “the same” if the predicate `equal?`{.language-plaintext
.highlighter-rouge} return `#t`{.language-plaintext .highlighter-rouge}
when given the two values as its arguments.

For full credit you **may not** use global state.

Hint: Write a version using global state first. Then review the bank
account example from the state lecture and modify your example so that
it uses only local state.



