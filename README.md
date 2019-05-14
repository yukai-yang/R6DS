<!-- README.md is generated from README.Rmd. Please edit that file -->

R6DS version 1.1.1 (Red DS)
===========================

R6DS stands for R6 class based Data Structures. The package provides
reference classes implementing some useful [data
stuctures](https://en.wikipedia.org/wiki/Data_structure). They are:

-   RStack
    ([stack](https://en.wikipedia.org/wiki/Stack_(abstract_data_type)))
-   RQueue
    ([queue](https://en.wikipedia.org/wiki/Queue_(abstract_data_type)))
-   RDeque ([double-ended
    queue](https://en.wikipedia.org/wiki/Double-ended_queue))
-   RDLL ([doubly linked
    list](https://en.wikipedia.org/wiki/Doubly_linked_list))
-   RSet ([set](https://en.wikipedia.org/wiki/Set_(abstract_data_type)))
-   RDict
    ([dictionary](https://en.wikipedia.org/wiki/Associative_array))
-   RBST ([binary search
    tree](https://en.wikipedia.org/wiki/Binary_search_tree))

How to install
--------------

You can install the development version from GitHub

``` r
devtools::install_github("yukai-yang/R6DS")
```

provided that the package “devtools” has been installed beforehand.

Get started
-----------

After installing the package, you need to load (attach better say) it by
running the code

``` r
library(R6DS)
```

You can first check the information and the current version number by
running

``` r
version()
#> R6DS version 1.1.1 (Red DS)
```

Then you can take a look at all the available functions and data in the
package

``` r
ls( grep("R6DS", search()) ) 
#> [1] "RBST"    "RDeque"  "RDict"   "RDLL"    "RQueue"  "RSet"    "RStack" 
#> [8] "version"
```

Now you can dive deeply into the package by reading the manual

``` r
?R6DS
```

Enjoy!

Why do we need the reference class?
-----------------------------------

I do not want to spend time on explaining why the data structures
implemented in the package are important. The importance of the data
structures in algorithm implementation and design patterns has been
mentioned many many times in textbooks. In this section, I would like to
explain why we need to combine the idea of the reference class and the
data structures in R.

### The reference class

The first question is: what is the reference class?

As an R user, you should know that, in R, everything is an object. This
includes the variables like vector, list, data.frame, and etc., and even
all the functions (closures).

Almost all the variables (if the functions are also variables, yes, they
are, because the function name can be changed to another function) are
assigned or passed by value (this is termed pass-by-value).

For example, consider the simple assignment:

``` r
x <- 0
y <- x
```

R will copy the value of `x` to `y`, and they are supposed to be two
different variables which occupy different memory pieces. However, in
practice, the new memory will not be allocated for `y` immediately when
running `y <- x`. R will do a fake pass-by-reference (that we will
explain later), and later new memory will be allocated for `y`(or `x`),
once the value of `x`(or `y`) is changed. So anyway, `x` to `y` are
supposed to be in different places in the memory.

Now consider the pass:

``` r
# define a function which changes the value of the passed variable,
# and then the new one
func <- function(val){ val <- -1; return(val) }

# remember that x = 0
z <- func(x)
x
#> [1] 0
z
#> [1] -1
```

R will copy the variable `x` to a new variable `val`, change the value
of `val`, and then return it by value! (copy the local `val` to the new
variable `z`, and then kill `val` when the function exits) So the
function actually copies twice in both passing and returning.

It should be noticed that, if the value of `val` is not going to be
changed inside the function `func`, that is, `func` is defined to be a
immutable function like, for example,

``` r
# define a function which changes the value of the passed variable,
# and then the new one
func <- funcion(val){ print(val) }
```

then R will pass `val` by reference (that we will explain later). But
anyway, you are changing another variable `val` instead of `x` inside
the function.

The idea of pass-by-reference comes from a good dream that we hope when
we do something like

``` r
func <- function(val){ val <- -1 }
func(x)
```

the global `x` will be changed…

I will explain why it is a good dream later. But now let’s focus on the
pass-by-reference.

The pass-by-reference in assignment implies that, in the assignment
action, for example,

``` r
y <- x
```

the variable `y` will be an alias (the term is a “reference”) of `x`,
which means that both `x` and `y` will share the same memory. Once you
change the value of either of the two, the other will be changed
automatically.

Now consider the function

``` r
func <- function(val){ val <- -1 }
func(x)
```

If the function is pass-by-reference, then argument `val` will share the
same memory of `x` when you run `func(x)`, and the value of the global
`x` will be changed after calling the function.

So when we do pass-by-reference, no new memory is allocated, the
original variable will have an alias. When you change the value of the
alias, the original one will also be changed.

The reference class follows the same rule, that is, any object or
instance of this class will always be passed-by-reference.

### Data structures

R is becoming a more and more sophisticated language. R users program by
using R to make achievements in data sciences. Now the question is that
“Are you really happy with only pass-by-value?” or “whether only the
pass-by-value is sufficient?”

The answer is NO, because we are doing much more than expected in data
sciences, and some ideas from the data structures are now needed, but
the implementation of these ideas needs the pass-by-reference!

So the dream is that **“we hope that we can input something into some
function and the function will change its value!”**

Or, for some people, **“we really miss the pointers or references in
Fortran, C, C++, and etc.”**

Even R says no itself due to the fact the Reference Class following the
S4 class has been implemented. And then the much more efficient R6 class
was implemented and available in the R6 package. I employ the R6 class
to implement the data structures.

Suppose that you want to design and implement some algorithm. The most
efficient solution is, for example, to use the recursion, in which you
pass a variable into some function, the function will call itself and
pass the same variable into it. The corresponding algorithm requires
that the variable will be changed inside the recursion. See for example,
the [traverse algorithm in the
binary-search-tree](https://en.wikipedia.org/wiki/Binary_search_tree),
in which, if we want to have a copy of these traversed elements, then it
is desirable to pass-by-reference a container (vector, list, data.frame
and etc.) into the recursive traverse function. Once an element in the
tree is reached, its value will be copied into the container.

Now you see that, the data structures and the algorithms do require the
pass-by-reference feature!

The R6 class is truly the reference class
-----------------------------------------

In order to investigate the R6 class, especially to confirm that it is
doing pass-by-reference, consider the example below

``` r
library(R6)

RClass <- R6Class("RClass", portable = FALSE, class = FALSE)

RClass$set("private", ".val", 0)

RClass$set("public", "initialize", function(val=0){ .val <<- val })

RClass$set("active", "Val", function(){ return(.val) })

RClass$set("public", "Set", function(newval){ .val <<- newval })

RClass$set("public", "finalize", function(){ print(paste("obj",.val,"deleted!")) })
```

We define a new R6 type reference class `RClass`. It is has a `finalize`
function which will be run when the system removes the instance of the
class (free or collect the memory allocated for it). We see that the
`finalize` will print a message show that the instance is being deleted.
We do the memory garbage collection manually by using the `gc` function,
because R is “lazy”…

We test if the memory will be collected when we run `gc`.

``` r
tmp1 = RClass$new()
rm(tmp1)

gc()
#> [1] "obj 0 deleted!"
#>           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
#> Ncells  837263 44.8    1653898 88.4         NA  1155422 61.8
#> Vcells 1464422 11.2    8388608 64.0      16384  2309590 17.7
```

Yes.

Then we define the first function to check if the memory of the local
variable will be freed when the function exits.

``` r
ftmp <- function(){ tmp <- RClass$new() }
ftmp()

gc()
#> [1] "obj 0 deleted!"
#>           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
#> Ncells  839417 44.9    1653898 88.4         NA  1253622 67.0
#> Vcells 1470956 11.3    8388608 64.0      16384  2309590 17.7
```

For sure, it will.

We override the same function to take a variable and print it.

``` r
ftmp <- function(tmp){ tmp$Set(1) }
```

Our experiment is designed as follows. If the instance of the `RClass`
class is passed-by-value into the function `ftmp`, then i) the global
`tmp1` will not be changed; ii) the new variable `tmp` inside the
function will be removed when the function exits, which means that we
will see some message saying “obj 1 deleted!”, because the value of
`tmp` is set to one. Now let’s check

``` r
tmp1 = RClass$new()
ftmp(tmp1)

gc()
#>           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
#> Ncells  839463 44.9    1653898 88.4         NA  1253622 67.0
#> Vcells 1471132 11.3    8388608 64.0      16384  2309590 17.7
tmp1$Val
#> [1] 1
```

We see that no memory space is freed, which means that inside the
function, no new variable was created. The value of the global `tmp1`
has been changed to one successfully.

So our conclusion is that R6 is capable!

The binary search tree example
------------------------------

Here is one example of the package R6DS. The binary search tree is quite
efficient in sorting, searching and traversing its elements. The time
complexity can be *O*(*l**o**g**n*) if the tree is well structured.

When building the binary search tree, the “\<” and “=” operators should
be defined and equip to the generator instance of the `RBST` class. We
just compare the numbers for simplicity.

We consider the tree as follows

![](https://upload.wikimedia.org/wikipedia/commons/d/da/Binary_search_tree.svg)

Graph source: [Binary Search Tree @
WIKIPEDIA](https://en.wikipedia.org/wiki/Binary_search_tree).

We try the package:

``` r
# we define the "<" and "="
lessthan <- function(x, y) return(x < y)
equal <- function(x, y) return(x == y)
# then we initialize the tree
bst <- RBST$new(lessthan=lessthan, equal=equal)

# the nodes in a vector
nodes <- c(8, 3, 10, 1, 6, 14, 4, 7, 13)
# we add the nodes or elements
bst$insert(collapse = as.list(nodes))
```

OK, the tree is now built. When you read the manual of the `RBST` class,
you will see that we can do traversal by calling the `traverse` function
of the class. Each node or element can be manipulated by using the
function `callback` that is input into `traverse`. But it is a really
good dream that we can pass something by reference into the recursive
`traverse` and do something inside the recursion.

Now the dream comes true. We do:

``` r
# create an empty container to hold the elements
# we choose the data structure queue
container <- RQueue$new()
container$size
#> [1] 0

# then we define the callback function which takes two arguments
# node: the node or element in the tree
# queue: additional argument hopefully pass-by-reference
callback <- function(item, queue) queue$enqueue(item)
# note that item is the value of the node but not the node!

# traverse-in-order
bst$traverse(mode="in", callback=callback, container)
# it should be a sorted list of the elements in the tree
# 1  3  4  6  7  8 10 13 14
unlist(container$toList)
#> [1]  1  3  4  6  7  8 10 13 14

# empty the container
container <- RQueue$new()
container$size
#> [1] 0

# traverse-pre-order
bst$traverse(mode="pre", callback=callback, container)
# it should be
# 8  3  1  6  4  7 10 14 13
unlist(container$toList)
#> [1]  8  3  1  6  4  7 10 14 13

# empty the container
container <- RQueue$new()
container$size
#> [1] 0

# traverse-post-order
bst$traverse(mode="post", callback=callback, container)
# it should be 
# 1  4  7  6  3 13 14 10  8
unlist(container$toList)
#> [1]  1  4  7  6  3 13 14 10  8
```

You can pass any instance of some reference class into the function to
make the manipulation of the data much more flexible. And you should see
how important the package can be…

But of course, the example is somewhat “stupid” as you can solve the
same problem by using some global variable. However, global variable
should not be preferred in many other cases, for example: when you want
to share your code with others, and preferrably the function offers the
only interface (they do not need to know the names of the global
variables and create them manually)
