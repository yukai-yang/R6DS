##########################################################################################
# Set
# a set is a linked list equipped with equal function
##########################################################################################

#' The RSet reference class
#'
#' The RSet reference class implements the data structure set.
#'
#' A set is a collection of items or elements equipped with the "=" operators
#' such that any two elements in the set cannot be equal.
#' The set data structure does not care the order of the elements.
#'
#' The class \code{RSet} inherits the \code{\link{RDLL}} class,
#' and therefor it has all the methods that \code{\link{RDLL}} has.
#'
#' Note that the methods \code{insert_at}, \code{appendleft}, \code{append}
#' in the super class still works without checking if the new element equals
#' any other elements in the set.
#' Normally they should be depreciated in the \code{RSet} class,
#' but this is not done in the current version of the package.
#' It is strongly recommended that the user should use the \code{add} method
#' to add a new element when using the \code{RSet} class.
#'
#' The elements in the set are not necessarily to be of the same type,
#' and they can even be of function type.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the set data structure, see \href{https://en.wikipedia.org/wiki/Set_(abstract_data_type)}{Set at Wikipedia}.
#'
#' @seealso \link{RDLL} and \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Class Method:
#'
#' The class method belongs to the class.
#'
#' \describe{
#'
#' \item{\code{new(equal, ..., collapse=NULL)}}{
#' The \code{new} method creates a new instance of the RSet class
#' containing the values in \code{...} and \code{collapse} as its elements.
#'
#' The argument \code{equal} takes a function defining the "=" operation,
#' The function set to \code{equal} takes two values of the elements in the set and return
#' a boolean.
#' It can be, for example, of the form
#'
#' \code{equal <- function(x, y) return(x$num == y$num)}
#'
#' where \code{x} and \code{y} are values of two elements in the set
#' with the attribute \code{num}.
#' }
#'
#' }
#'
#' @section Immutable Methods:
#'
#' The immutable methods do not change the elements of the instance.
#'
#' \describe{
#'
#' \item{\code{has(val)}}{
#' The method \code{has} returns a boolean indicating
#' if the set contains \code{val}.
#' }
#'
#' \item{\code{union(rset)}}{
#' The method \code{union} takes another instance \code{rset} of the \code{RSet} or \code{RDLL} class
#' and returns the union of the two sets.
#' }
#'
#' \item{\code{intersection(rset)}}{
#' The method \code{intersection} takes another instance \code{rset} of the \code{RSet} class
#' and returns the intersection of the two sets.
#' }
#'
#' \item{\code{difference(rset)}}{
#' The method \code{difference} takes another instance \code{rset} of the \code{RSet} class
#' and returns the difference (current instance minus \code{rset}) of the two sets.
#' }
#'
#' \item{\code{subset(rset)}}{
#' The method \code{subset} takes another instance \code{rset} of the \code{RSet} class.
#' It returns \code{TRUE} if the current instance is a subset of \code{rset}.
#' }
#'
#' \item{\code{contains(rset)}}{
#' The method \code{contains} takes another instance \code{rset} of the \code{RSet} or \code{RDLL} class.
#' It returns \code{TRUE} if the current instance contains \code{rset}.
#' }
#'
#' }
#'
#' @section Mutable Methods:
#'
#' The mutable methods changes the elements of the instance.
#'
#' \describe{
#'
#' \item{\code{add(val)}}{
#' The method \code{add} adds a new element and returns \code{TRUE}
#' showing that the inserting is successful,
#' but if there is one element in current set that is \code{equal} to the element,
#' the method will do nothing and return a \code{FALSE}
#' showing that the adding fails.
#' }
#'
#' \item{\code{delete(val)}}{
#' The method \code{delete} removes the element which is \code{equal} to \code{val}.
#' If the element is found, then it will be removed and the function returns a \code{TRUE},
#' and if the element is not found, then it will do nothing and returns a \code{FALSE},
#' }
#'
#' }
#'
#' @keywords RSet
#'
#' @examples
#'
#' ### create a new instance
#'
#' # you have to define "="
#' equal <- function(x, y) return(x$key == y$key)
#' # remember that the elements in the set must have the "key" attribute
#'
#' # to create a new instance of the class
#' set <- RSet$new(equal=equal)
#'
#' # of course you can start to add elements when creating the instance
#' # the previous RSet instance will be removed by doing so
#' # and the memory allocated for that one will be cleared,
#' # as now set has been pointed to another instance of the class.
#' set <- RSet$new(equal=equal,
#'     list(key=5, val="5"), collapse=list(list(key=3,val="3"), list(key=9,val="9")))
#' # the following sentence is equivalent to the above
#' set <- RSet$new(equal=equal,
#'     list(key=5, val="5"), list(key=3,val="3"), list(key=9,val="9"))
#' # where the three lists are inserted into the set
#'
#' ### immutable methods
#'
#' set$has(list(key=5, num=10))
#' # TRUE as it has the key attribute
#'
#' ### mutable methods
#'
#' set$add(list(key=5, num=10))
#' # FALSE
#'
#' set$add(list(key=10, val="10"))
#' # TRUE
#'
#' set$delete(list(key=10))
#' # TRUE and list(key=10, val="10") is removed
#'
#' # union
#' another_set <- RSet$new(equal=equal,
#'     list(key=5, val="5"), list(key=11,val="11"))
#' set$union(another_set)$show()
#'
#' # intersection
#' set$intersection(another_set)$show()
#'
#' # difference
#' set$difference(another_set)$show()
#'
#' # subset
#' set$subset(another_set)
#'
#' # contains
#' set$contains(another_set)
#'
#' @export
RSet <- R6Class("RSet", inherit = RDLL, portable = FALSE, class = FALSE)

RSet$set("private", "equal", NULL)

RSet$set("public", "initialize", function(equal, ..., collapse=NULL){
  equal <<- equal
  items = c(list(...), as.list(collapse))

  for(item in items) add(item)
})

RSet$set("public", "has", function(val){
  current <- .head
  while(!is.null(current)){
    if(equal(current$Val, val)) return(TRUE)
    current <- current$Next
  }
  return(FALSE)
})

RSet$set("public", "add", function(val){
  if(!has(val)){ super$append(val); return(TRUE)}
  return(FALSE)
})

RSet$set("public", "delete", function(val){
  current <- .head
  while(!is.null(current)){
    if(equal(current$Val, val)){

      if(is.null(current$Prev)){
        .head <<- current$Next
      }else{
        current$Prev$setNext(current$Next)
      }
      if(is.null(current$Next)){
        .tail <<- current$Prev
      }else{
        current$Next$setPrev(current$Prev)
      }

      .len <<- .len-1; return(TRUE)
    }
    current <- current$Next
  }
  return(FALSE)
})

RSet$set("public", "union", function(rset){
  ret <- RSet$new(equal=equal,collapse=self$toList)
  items <- rset$toList
  for(item in items) ret$add(item)
  return(ret)
})

RSet$set("public", "intersection", function(rset){
  ret <- RSet$new(equal=equal)
  items <- rset$toList
  for(item in items) if(has(item)) ret$append(item)
  return(ret)
})

RSet$set("public", "difference", function(rset){
  ret <- RSet$new(equal=equal)
  items <- self$toList
  for(item in items) if(!rset$has(item)) ret$append(item)
  return(ret)
})

# rset must be a set
RSet$set("public", "subset", function(rset){
  return(rset$contains(self))
})

RSet$set("public", "contains", function(rset){
  items <- rset$toList
  for(item in items) if(!has(item)) return(FALSE)
  return(TRUE)
})