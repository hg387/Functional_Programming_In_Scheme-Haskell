Problems_Description

### Interval Sets

For this homework, you will implement and test interval sets.

Interval sets are a way of representing sets of integers. Sets are
represented as a collection of intervals, and intervals are represented
by a pair of integers. The type of interval sets that we will use in
Haskell is:

``` {.highlight}
newtype IntSet = IntSet [(Int,Int)]
  deriving (Eq, Ord, Show)
```

For example, to represent the set {2,3,4,10,11,12,13}{2,3,4,10,11,12,13}

we use `IntSet [(2,4),(10,13)]`{.language-plaintext .highlighter-rouge}.
The interval `(2,4)`{.language-plaintext .highlighter-rouge} means that
we have 2,3,42,3,4

in the set; the interval `(10,13)`{.language-plaintext
.highlighter-rouge} means that we have 10,11,12,1310,11,12,13

in the set.

The intervals used in interval sets should satisfy the following
invariants:

1.  For any interval `(x,y)`{.language-plaintext .highlighter-rouge} in
    a set, `x`{.language-plaintext .highlighter-rouge} should not be
    greater than `y`{.language-plaintext .highlighter-rouge} (so the
    intervals are not empty); for example `(2,5)`{.language-plaintext
    .highlighter-rouge} and `(4,4)`{.language-plaintext
    .highlighter-rouge} are OK, but not `(7,2)`{.language-plaintext
    .highlighter-rouge}.

2.  Two intervals in the same set should not be overlapping (or even
    touching); for example we cannot have the intervals
    `(2,6)`{.language-plaintext .highlighter-rouge} and
    `(4,10)`{.language-plaintext .highlighter-rouge} in the same set;
    they should be replaced by `(2,10)`{.language-plaintext
    .highlighter-rouge}. We cannot have `(3,6)`{.language-plaintext
    .highlighter-rouge} and `(7,11)`{.language-plaintext
    .highlighter-rouge} in the same set either; they should be replaced
    by `(3,11)`{.language-plaintext .highlighter-rouge}

3.  The intervals in a given interval set should occur in ascending
    order; for example
    `IntSet [(1,3),(5,9),(15,16)]`{.language-plaintext
    .highlighter-rouge} is fine, but
    `IntSet [(5,10),(1,3)]`{.language-plaintext .highlighter-rouge} is
    not.

### Using `ghci`{.language-plaintext .highlighter-rouge} for the assignment

You must use `stack`{.language-plaintext .highlighter-rouge} to start
`ghci`{.language-plaintext .highlighter-rouge} to ensure the appropriate
modules are loaded. Assuming you want to use the testing infrastructure
from `ghci`{.language-plaintext .highlighter-rouge}, start
`ghci`{.language-plaintext .highlighter-rouge} using this command:

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

### Problem 1: Implementation (75 points total)

Implement the following functions on interval sets, keeping the above
properties in mind:

``` {.highlight}
empty  :: IntSet                        -- | The empty set
member :: Int -> IntSet -> Bool         -- | Checking for set membership
insert :: Int -> IntSet -> IntSet       -- | Insert an element
merge  :: IntSet -> IntSet -> IntSet    -- | Set union
delete :: Int -> IntSet -> IntSet       -- | Delete an element
```

The functions should be robust; for example inserting an element that
already exists (or deleting an element that does not exist) should
simply return the original set.

You **may not** internally convert intervals to lists of integers, work
with these lists, and then convert back to intervals—you **must**
operate directly on the intervals themselves. If your implementation of
any of the five functions above uses lists of integers in any way,
rethink your solution. However, you **will** need to manipulate lists of
*intervals*! In short, lists of *integers* bad, list of *intervals*
good.

Please put your implementation in the file
`src/IntSet.hs`{.language-plaintext .highlighter-rouge} which we have
provided. You **must not** change the definition of the
`IntSet`{.language-plaintext .highlighter-rouge} type or the type
signatures of the above functions.

You will likely want to solve problem 1 and problem 2
simultaneously—your tests will help you write a correct implementation.

I suggest you write and test `empty`{.language-plaintext
.highlighter-rouge} and `member`{.language-plaintext .highlighter-rouge}
first. Then move on to writing `delete`{.language-plaintext
.highlighter-rouge} and its test(s) next. Finally, tackle
`insert`{.language-plaintext .highlighter-rouge},
`merge`{.language-plaintext .highlighter-rouge}, and their tests.

Hint: Write `insert`{.language-plaintext .highlighter-rouge} in terms of
`merge`{.language-plaintext .highlighter-rouge}. How do you represent an
`IntSet`{.language-plaintext .highlighter-rouge} containing a single
value?

### Problem 2: Testing (25 points total)

Write unit tests for all five of your functions above. Put your unit
tests in the file `test/IntSetUnitSpec.hs`{.language-plaintext
.highlighter-rouge}. Test coverage is worth 5 points per function; you
will not receive credit for the one unit test we provide. You will
likely want to use the function `fromList`{.language-plaintext
.highlighter-rouge} from `test/IntSet.hs`{.language-plaintext
.highlighter-rouge}.

We have provided one example test so that you can see the syntax for
writing tests. We use the Hspec framework for testing, which you can
read about [on the Hspec web site](https://hspec.github.io/).

You should provide **at least** one test for each function. Ideally, you
should write tests as you develop your implementation, and by the end of
the assignment, you will have a fleshed-out test suite as a side-effect
of the development process. Don’t view the tests as something you have
to write after the fact just to finish the assignment. A good strategy
is to ensure every category/partition of inputs is covered by your unit
tests.

#
