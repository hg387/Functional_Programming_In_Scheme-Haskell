Problems_Description

In this assignment, you will modify the metacircular interpreter we saw
in class. Successfully completing this assignment requires reading and
understanding a medium-sized program written by someone else. If you do
not have a good understanding of how the interpreter works, please
review the material covered in lecture and in the book. Diving straight
in to the homework without a good understanding of the fundamentals is
going to make your task much more difficult.

You should complete all problems by modifying the
`README.md`{.language-plaintext .highlighter-rouge},
`mceval.rkt`{.language-plaintext .highlighter-rouge}, and
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge} files in your
repository. **Do not** create additional copies of
`mceval.rkt`{.language-plaintext .highlighter-rouge} or
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge} for the
different parts of the assignment. You may modify other files, including
the test files, but we will grade only your
`README.md`{.language-plaintext .highlighter-rouge},
`mceval.rkt`{.language-plaintext .highlighter-rouge}, and
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge} files.

Problems 1 and 2 require you to modify the applicative-order interpreter
in `mceval.rkt`{.language-plaintext .highlighter-rouge}. You will want
to start with the `mceval.rkt`{.language-plaintext .highlighter-rouge}
interpreter you wrote for the previous assignment.

Problems 3 and 4 require you to modify the lazy interpreter,
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge}.

**This assignment is worth 100 points. There are 120+10 possible
points.**

### Working with the interpreter

There are two ways you can test your evaluator:

1.  Type make and run the resulting binary, named
    `mceval`{.language-plaintext .highlighter-rouge}. The
    `mceval`{.language-plaintext .highlighter-rouge} program will
    repeatedly read in a Scheme expression and pass it to your
    interpreter for evaluation.
2.  From DrRacket, call the `top-mceval`{.language-plaintext
    .highlighter-rouge} function with an expression, like this:
    `(top-mceval '(+ 2 3))`{.language-plaintext .highlighter-rouge}.
    **Make sure you quote the expression you want to evaluate**.

The second method is preferred because it allows you to use the GUI
debugger to set breakpoints and step through code. Read the [Graphical
Debugging
Interface](https://docs.racket-lang.org/drracket/debugger.html) portion
of the Racket manual for more information. If the debugger seems “stuck”
the first time you click “Debug,” click the “Go” button.

If you are *not* using DrRacket, you will find the [Racket debugging
reference
helpful](https://download.racket-lang.org/releases/7.9/doc/reference/debugging.html).

No matter how you run the evaluator, you can always use
`display`{.language-plaintext .highlighter-rouge} and
`newline`{.language-plaintext .highlighter-rouge} to print out
intermediate expressions when debugging.

#### Running the Test Suite

To run the tests we provide using the GUI, open the file
`run-tests-gui.rkt`{.language-plaintext .highlighter-rouge} in DrRacket
and click the “Run” button. As with all other assignments, you may also
use `make test`{.language-plaintext .highlighter-rouge}.

### Hints for solving the problems

There are three primary ways to extend the interpreter:

1.  Add a primitive.
2.  Add a definition to the global environment.
3.  Add a special form.

You should only add a special form when it is absolutely necessary. Most
of the time, the standard Scheme evaluation rules are exactly what you
want. Solving a problem by adding a definition rather than a new special
form is also much easier and avoids cluttering up your
`mceval`{.language-plaintext .highlighter-rouge} function.

\*\*Make sure you read the “Course Notes on the Metacircular Evaluator!”
They tell you exactly how to implement a primitive, a top-level
definition, and a special form.

It is **very** useful to define helper functions that can be tested
independently of the rest evaluator, especially when implementing a
special form as a derived expression.

Problem 1: Implementing streams (30 points total)
-------------------------------------------------

Add support for the following stream functions to your interpreter:

1.  `stream-cons`{.language-plaintext .highlighter-rouge} (8 points)
2.  `empty-stream`{.language-plaintext .highlighter-rouge} (2 points)
3.  `stream-empty?`{.language-plaintext .highlighter-rouge} (2 points)
4.  `stream-first`{.language-plaintext .highlighter-rouge} (4 points)
5.  `stream-rest`{.language-plaintext .highlighter-rouge} (4 points)

You should be able to complete this problem with either the call-by-name
or call-by-need implementation of `force`{.language-plaintext
.highlighter-rouge} and `delay`{.language-plaintext .highlighter-rouge}.
That is, you can receive full credit for this problem even if you did
not receive full credit for Problem 4.

Your streams should be lazy in both the head of the stream and the tail,
like Racket. Note that in SICP, streams are lazy in **only** the tail.

Your implementation must use your `force`{.language-plaintext
.highlighter-rouge} and `delay`{.language-plaintext .highlighter-rouge}
from Problem 4. You may not use Racket’s `force`{.language-plaintext
.highlighter-rouge} and `delay`{.language-plaintext .highlighter-rouge}
or equivalent syntax macros in your solution to this problem.

Problem 2: Implement `stream`{.language-plaintext .highlighter-rouge}(30 points total)
--------------------------------------------------------------------------------------

`(stream expr ...)`{.language-plaintext .highlighter-rouge} is shorthand
for nested `stream-cons`{.language-plaintext .highlighter-rouge}es
ending with `empty-stream`{.language-plaintext .highlighter-rouge}.

Hint: Implement this as a derived expression.

Problem 3: Extending the lazy evaluator (20 points total)
---------------------------------------------------------

For this problem, you will modify the **lazy** evaluator in
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge}.

Add your primitives and `and`{.language-plaintext .highlighter-rouge}
and `or`{.language-plaintext .highlighter-rouge} to the lazy evaluator.
You should *almost* be able to copy the code over as-is. Be careful with
`and`{.language-plaintext .highlighter-rouge} and
`or`{.language-plaintext .highlighter-rouge} (why?).

Problem 4: A Hybrid Evaluator (40 points total)
-----------------------------------------------

For this problem, you will modify the **lazy** evaluator in
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge}.

The original metacircular evaluator in `mceval.rkt`{.language-plaintext
.highlighter-rouge} implements *applicative-order evaluation*—arguments
are evaluated before a function is called. The lazy evaluator in
`lazy-mceval.rkt`{.language-plaintext .highlighter-rouge} implements
*normal-order evaluation*—arguments are evaluated when they are needed.
For this problem, you will modify the lazy evaluator to implement a
**hybrid** approach: the *definition* of each function will specify
which arguments are delayed and which are evaluated immediately. To
illustrate, consider the following function we saw in lecture:

``` {.highlight}
(define (try a b)
  (if (= a 0) 1 b))
```

Evaluating `(try 0 (/ 1 0))`{.language-plaintext .highlighter-rouge}
will produce an error in the applicative-order interpreter. The problem
is that the applicative-order interpreter evaluates all function
arguments, even when they are not needed. Imagine we could write this
instead (this **is not** standard Scheme!)

``` {.highlight}
(define (try a (delayed b))
  (if (= a 0) 1 b))
```

Where `(delayed b)`{.language-plaintext .highlighter-rouge} is an
annotation to change the way compound procedures are applied as follows:

1.  First evaluate the operator.
2.  For each argument, if the corresponding parameter is annotated with
    `delayed`{.language-plaintext .highlighter-rouge}, delay its
    evaluation.
3.  Otherwise, evaluate it.

In the case of `try`{.language-plaintext .highlighter-rouge}, this would
mean that `a`{.language-plaintext .highlighter-rouge} is evaluated and
`b`{.language-plaintext .highlighter-rouge} is delayed.

Modifying the interpreter to support this extension takes very little
code, but it requires that you understand the metacircular interpreter
and lazy evaluation.

Problem 5: Homework Statistics (10 points total)
------------------------------------------------

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


