<!-- README.md is generated from README.Rmd. Please edit that file -->

R6DS version 1.1.1 (Red DS)
===========================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/R6DS)](https://cran.r-project.org/package=R6DS)
![](http://cranlogs.r-pkg.org/badges/grand-total/R6DS)
![](http://cranlogs.r-pkg.org/badges/R6DS)
![](http://cranlogs.r-pkg.org/badges/last-week/R6DS)

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

For an introduction of the package, please read the online vignette

[Introduction to the R6DS
Package](https://github.com/yukai-yang/R6DS/blob/master/R6DS-vignette.md)

How to install
--------------

You can either install the stable version from CRAN

``` r
install.packages("R6DS")
```

or install the development version from GitHub

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

Something need to be clarified!
-------------------------------

It is quite straightforward to create a new instance of the class in the
package. What you can do, for example, is to use the `new` function

``` r
rstack <- RStack$new()
```

and an empty stack (`rstack`) will be initialized.

You can push elements into it.

``` r
rstack$push(1, 2, 3)
```

and even heterogeneous elements

``` r
rstack$push("Hello world!", list(key=1, val=2), RQueue$new())
```

Notice that, the last pushed element is an instance of the class
`RQueue` in the package.

Remember that, in R, only the assignment or pass of an instance of some
reference class is pass-by-reference! In the following sentence,
`rstack` pops the last stacked element (return and remove its handle in
`rstack`) and assign it by-reference to `rqueue`

``` r
rqueue <- rstack$pop()
```

And the following assignments are pass-by-value (a copy).

``` r
rlist <- rstack$pop()
rstring <- rstack$pop()
```

The difference between the two assignments are:

-   `rqueue` shares the same memory with the used-to-be-the-last (but
    not ever since the pop) element in `rstack`, and R did not allocate
    memory space when creating `rqueue`.
-   `rlist`and `rstring` are variables with newly allocated memory
    spaces. As for the list and string elements in `rstack`, they have
    been removed completely.

So the conclusion is that, whether it is a pass-by-value or by-reference
depends on the object to be passed, not anything else.
