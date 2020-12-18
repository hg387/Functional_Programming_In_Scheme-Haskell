Problems_Description

**You may continue to work with the same 1–2 other students your worked
with on Project 2**. You **must acknowledge all collaboration** in your
`README.md`{.language-plaintext .highlighter-rouge} file. Be sure to
adhere to the [collaboration
policy](https://www.cs.drexel.edu/~mainland/courses/cs550-202035/collaboration.pdf)
you signed for Homework 0. Remember, **all work must be your own**.

**This assignment is worth 50 points. There are 55 possible points.**

In addition, you may **earn back** the points you lost on Problem 1 of
Project 2 by using your property-based tests to correct your
implementation of interval sets. However, you may only earn back those
points **if you implemented the relevant portion of Project 2**. For
example, if you did not implement `merge`{.language-plaintext
.highlighter-rouge}, you cannot earn back points for
`merge`{.language-plaintext .highlighter-rouge}.

### Using `ghci`{.language-plaintext .highlighter-rouge} for the assignment

You can use `make test`{.language-plaintext .highlighter-rouge} to test
as usual, but you may also want to develop interactively with
`ghci`{.language-plaintext .highlighter-rouge}. If so, these
instructions are for you.

You must use `stack`{.language-plaintext .highlighter-rouge} to start
`ghci`{.language-plaintext .highlighter-rouge} to ensure the appropriate
modules are loaded using this command:

``` {.highlight}
TMPDIR=/tmp/$USER stack repl --test
```

You can then run the unit tests as follows in `ghci`{.language-plaintext
.highlighter-rouge}:

``` {.highlight}
> hspec IntSetUnitSpec.spec
```

After you modify your implementation, reload it in
`ghci`{.language-plaintext .highlighter-rouge} as follows:

``` {.highlight}
> :reload
```

Then you can test again by calling `hspec`{.language-plaintext
.highlighter-rouge} as above.

You can run your property-based tests using a slightly different
command:

``` {.highlight}
> hspec IntSetPropertySpec.spec
```

#### Increasing the number of tests

If you examine the `Makefile`{.language-plaintext .highlighter-rouge},
you will see that `make test`{.language-plaintext .highlighter-rouge}
does nothing more than run `stack test`{.language-plaintext
.highlighter-rouge}. You can pass extra command-line arguments to the
test using the `--test-arguments`{.language-plaintext
.highlighter-rouge} option of `stack test`{.language-plaintext
.highlighter-rouge}. For example, to increase the number of QuickCheck
tests run from 100 (the default) to 1000, invoke
`stack test`{.language-plaintext .highlighter-rouge} as follows:

``` {.highlight}
stack test --test-arguments --qc-max-success=1000
```

You can increase the maximum size of the test instances as follows:

``` {.highlight}
stack test --test-arguments --qc-max-size=100
```

You can even combine both options like this:

``` {.highlight}
stack test --test-arguments --qc-max-success=1000 --test-arguments --qc-max-size=100
```

### Problem 1: Property-based Testing (50 points total)

You must complete the implementations of the following QuickCheck
properties in `test/IntSetPropertySpec.hs`{.language-plaintext
.highlighter-rouge}. Each test is worth 10 points.

``` {.highlight}
prop_empty :: Bool
prop_member :: Int -> IntSet -> Bool
prop_merge :: IntSet -> IntSet -> Bool
prop_insert :: Int -> IntSet -> Bool
prop_delete :: Int -> IntSet -> Bool
```

You will want to copy your implementation of interval sets (and your
unit tests) from your Project 2 repository.

We have provided an `Arbitrary`{.language-plaintext .highlighter-rouge}
instance that you can use for testing and a function,
`model`{.language-plaintext .highlighter-rouge}, that takes a value of
type `IntSet`{.language-plaintext .highlighter-rouge} and returns a
*model* of the `IntSet`{.language-plaintext .highlighter-rouge}. This
model consists of a sorted list of integers in the
`IntSet`{.language-plaintext .highlighter-rouge}. Although the model is
an inefficient way to implement a set of integers, it is easy to reason
about. Your tests should use `model`{.language-plaintext
.highlighter-rouge} to relate operations on values of type
`IntSet`{.language-plaintext .highlighter-rouge} to operations on lists
of integers. The property you state should be necessary **and**
sufficient to guarantee a proper implementation. For example, your
`prop_merge`{.language-plaintext .highlighter-rouge} property must be
strong enough to guarantee that your `merge`{.language-plaintext
.highlighter-rouge} is correct. It is possible to state properties that,
although true for the correct implementation of an operation, are also
true for many other *incorrect* implementations of the same operation—we
saw an example of a “weak” property statement in lecture.

As an example, consider the `prop_insert`{.language-plaintext
.highlighter-rouge} test. It should test that the model of the
`IntSet`{.language-plaintext .highlighter-rouge} obtained by inserting a
value `x`{.language-plaintext .highlighter-rouge} into an
`IntSet`{.language-plaintext .highlighter-rouge} `s`{.language-plaintext
.highlighter-rouge} is the same as inserting the value
`x`{.language-plaintext .highlighter-rouge} into the *list* that is the
model of `s`{.language-plaintext .highlighter-rouge}.

Note that in the property-based test module, the
`IntSet`{.language-plaintext .highlighter-rouge} module has been
imported qualified under the name `IS`{.language-plaintext
.highlighter-rouge}, so, for example, the `member`{.language-plaintext
.highlighter-rouge} function for `IntSet`{.language-plaintext
.highlighter-rouge}s will be referred to as
`IS.member`{.language-plaintext .highlighter-rouge}. This will allow you
to use operations on lists that have the same name as the equivalent
operation on lists, e.g., `IS.insert`{.language-plaintext
.highlighter-rouge} for interval sets, and `insert`{.language-plaintext
.highlighter-rouge} for lists.

Problem 2: Fixing Your Interval Set Implementation (up to 75 points)
--------------------------------------------------------------------

Use your property-based tests from Problem 1 to fix your interval set
implementation from Project 2. You can earn back *all* of the points you
lost as long as you submitted Part I.

