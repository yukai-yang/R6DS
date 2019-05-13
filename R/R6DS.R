################################################################################
## package name: R6DS
## author: Yukai Yang
## Statistiska Inst., Uppsala Universitet
## May 2019
#################################################################################


#' R6DS: provides reference classes implementing some useful data stuctures.
#'
#' R6DS stands for \pkg{R6} class based Data Structures.
#' The package provides reference classes implementing some useful data stuctures.
#'
#' Some data structures are quite useful in solving some programming problems,
#' as they offer great convenience and are the keys to implement some algorithms.
#'
#' The package implements these data structures by using the reference class \pkg{R6}.
#' Each class defined in the package represents a certain data structure,
#' and it inherits the R6 reference class which means that it is also a reference class.
#'
#' In order to create an instance of the R6 type reference class, you will have to use its \code{new} method as follows:
#'
#' \code{instance <- RStack$new()}
#'
#' where \code{RStack} is an R6 type reference class.
#'
#' The reference class has the feature that each time when you pass (or assign) an instance of the reference class to somewhere,
#' it is not pass-by-copy but pass-by-reference. For example, suppose there is an instance of the reference class \code{x} who has an attribute \code{x$y}.
#' If you pass it to a function \code{func} by using \code{func(x)}, the function will not copy x but pass it as a reference.
#' Inside the function \code{func}, if there is a sentence like
#'
#' \code{x$y <- 0}
#'
#' then the attribute \code{y} of the global \code{x} outside the function will be changed to zero.
#'
#' Of course you can copy the instance of a reference class, but you have to use its \code{clone} method:
#'
#' \code{new_instance <- instance$clone()}
#'
#' Notice that all the classes in the package use instances of some other R6 type reference class as their members.
#' This implies that, according to the rule of the R6 family, you have to add \code{deep = TRUE} when you \code{clone} their instances:
#'
#' \code{new_instance <- instance$clone(deep=TRUE)}
#'
#' and then you can successfully copy them.
#'
#' The classes in the package are designed in the way that you cannot get the access to their members directly,
#' as they are declared to be private.
#' Instead, you have to use their methods (member functions) to get them.
#' In the following, a complete list of these classes and their methods in common are presented.
#' Each class has its own methods, and for details of these class-specific methods please refer to their help documents.
#'
#' Some methods are declared to be "active", or active method, which means that, when you call them, you do not need to use parenthesis.
#' For example, the \code{size} method is a common active method of all classes in the package. When you call it, you do
#'
#' \code{instance$size}
#'
#' So it looks pretty like a member attribute, but actually not.
#'
#' @section How to Use the Package:
#'
#' All classes in the package are declared to be non-portable and non-class (R6 standards),
#' which means that the user of the package cannot inherit them.
#'
#' The user can create and use instances of these classes,
#' and the instances can contain any R objects (vector, matrix, factor, data.frame, list and etc.) as their values.
#'
#' The author suggest that the user of the package puts the instances of these classes inside other classes
#' to be their members.
#'
#' But it is still possible to inherit the classes in the package.
#' To this end, the user can copy and paste the source code of the package.
#'
#'
#' @section Author and Maintainer:
#' Yukai Yang
#'
#' Department of Statistics, Uppsala University
#'
#' \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the data structures, see \href{https://en.wikipedia.org/wiki/Data_structure}{Data Structure at Wikipedia}.
#'
#' @section Classes Available in the Package:
#' \code{\link{RStack}} The RStack reference class implements the data structure stack.
#'
#' \code{\link{RQueue}} The RQueue reference class implements the data structure queue.
#'
#' \code{\link{RDeque}} The RDeque reference class implements the data structure double-ended queue.
#'
#' \code{\link{RDLL}} The RDLL reference class implements the data structure doubly linked list.
#'
#' \code{\link{RSet}} The RSet reference class implements the data structure set.
#'
#' \code{\link{RDict}} The RDict reference class implements the data structure dictionary.
#'
#' \code{\link{RBST}} The RBST reference class implements the data structure binary search tree.
#'
#' @section Common Methods of the Classes in the Package:
#'
#' \describe{
#' \item{\code{new(..., collapse=NULL)}}{
#' a method belonging to the class which create an instance of the class.
#' The method creates a new instance of some class in the package
#' containing the values in \code{...} and \code{collapse} as its elements.
#' }
#' \item{\code{clone(deep=FALSE)}}{
#' an immutable method of an instance which returns of copy of the instance.
#' The \code{deep=TRUE} argument indicates that \code{clone} also gives a copy of its members.
#' }
#' \item{\code{size}}{
#' an active immutable method of an instance to return its size (like the \code{length} of an R vector).
#' }
#'
#' }
#'
#' @docType package
#' @name R6DS
NULL

#' @importFrom R6 R6Class
NULL
