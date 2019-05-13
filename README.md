<!-- README.md is generated from README.Rmd. Please edit that file -->

R6DS version 1.1.0 (Red DS)
===========================

R6DS stands for R6 class based Data Structures. The package provides
reference classes implementing some useful data stuctures.

How to install
--------------

You can install the development version from GitHub

``` r
devtools::install_github("yukai-yang/R6DS")
```

provided that the package “devtools” has been installed beforehand.

Example
-------

After installing the package, you need to load (attach better say) it by
running the code

``` r
library(R6DS)
```

You can first check the information and the current version number by
running

``` r
version()
#> R6DS version 1.1.0 (Red DS)
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
