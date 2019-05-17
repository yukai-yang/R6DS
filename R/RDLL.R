##########################################################################################
# Double Linked list
##########################################################################################

#' The RDLL reference class
#'
#' The RDLL reference class implements the data structure doubly linked list (DLL).
#'
#' A doubly linked list is an ordered list of elements with multiple operations.
#' The DLL is a powerful sequantial data structure in the sense that
#' it can be regarded as the generalized version of the data structures stack, queue, deque.
#'
#' The class \code{RDLL} inherits the \code{\link{RDeque}} class,
#' and therefor it has all the methods that \code{\link{RDeque}} has.
#'
#' The DLL is much more friendly and flexible as it offers more useful methods to help the user get access to its elements
#' than \code{\link{RStack}}, \code{\link{RQueue}} and \code{\link{RDeque}}.
#' See below its immutable methods and mutable methods.
#'
#' It is worth noting that the classes \code{\link{RSet}} inherits the RDLL class,
#' and therefor it has all the methods that the RDLL has.
#'
#' The elements in the DLL are not necessarily to be of the same type,
#' and they can be any R objects.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the DLL data structure, see \href{https://en.wikipedia.org/wiki/Doubly_linked_list}{DLL at Wikipedia}.
#'
#' @seealso \link{RDeque}, \link{RSet}, and \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable methods do not change the instance.
#'
#' \describe{
#' \item{\code{show(callback=function(val){print(val)}, ...)}}{
#' The \code{show} method takes a funtion input (argument \code{callback})
#' specifying how to handle the elements in the DLL.
#' It also takes \code{...} as the additional arguments for the \code{callback} function if any.
#'
#' By default, the \code{show} method prints the elements by using the \code{print} function.
#'
#' \code{callback=function(val){print(val)}}
#'
#' You can see that \code{show} is powerful as it makes it possible to freely manipulate the elements in the DLL.
#' For example, you can define
#'
#' \code{func <- function(val, arg1, arg2){ do something here on val with arg1 and arg2 }}
#'
#' and then
#'
#' \code{instance$show(func, arg1, arg2)}
#'
#' And you can also store the elements by using instances of reference classes.
#' For example,
#'
#' \code{func <- function(val, queue){ queue$enqueue(val) }}
#'
#' where \code{queue} is an instance of \code{RQueue}. The code can be
#'
#' \code{queue <- RQueue$new()}
#'
#' \code{instance$show(func, queue)}
#' }
#'
#' \item{\code{elem_at(index)}}{
#' It returns the element (a copy) at position \code{index} (a positive integer).
#' \code{index} must be a scalar, and if it is a vector of more than one element,
#' only the first element will be considered.
#' If the value of \code{index} is out of the bounds of the instance,
#' a \code{NULL} will be returned.
#' }
#'
#' \item{\code{peekleft()}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' \item{\code{peek()}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' }
#'
#' @section Mutable Methods:
#'
#' The mutable methods change the instance.
#'
#' \describe{
#'
#' \item{\code{insert_at(index, val)}}{
#' This function inserts a new element \code{val} at position \code{index}.
#' It returns \code{TRUE} if the insertion is successful,
#' and \code{FALSE} if the \code{index} is out of the bounds.
#' It will push all the elements at and after \code{index} rightward.
#'
#' Thus, suppose that \code{instance} is an instance of the class.
#'
#' \code{insert_at(1, val)}
#'
#' is equivalent to \code{appendleft} in \code{\link{RDeque}}, and
#'
#' \code{insert_at(instance$size+1, val)}
#'
#' is equivalent to \code{append} in \code{\link{RDeque}},
#' \code{push} in \code{RStack}, and \code{enqueue} in \code{RQueue}.
#' }
#'
#' \item{\code{remove_at(index)}}{
#' This function returns and removes the element at position \code{index}.
#' It returns \code{NULL} if the \code{index} is out of the bounds.
#'
#' Thus, suppose that \code{instance} is an instance of the class.
#'
#' \code{remove_at(1, val)}
#' is equivalent to \code{popleft} in \code{\link{RDeque}}, and
#'
#' \code{remove_at(instance$size, val)}
#' is equivalent to \code{pop} in \code{\link{RDeque}} and \code{RStack},
#' and \code{dequeue} in \code{RQueue}.
#' }
#'
#' \item{\code{appendleft(..., collapse=NULL)}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' \item{\code{append(..., collapse=NULL)}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' \item{\code{popleft()}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' \item{\code{pop()}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' }
#'
#' @keywords RDLL
#'
#' @examples
#'
#' ### create a new instance
#'
#' # to create a new instance of the class
#' dll <- RDLL$new()
#'
#' # the previous RDLL instance will be removed if you run
#' dll <- RDLL$new(0, 1, 2, collapse=list(3, 4))
#' # the following sentence is equivalent to the above
#' dll <- RDLL$new(0, 1, 2, 3, 4)
#' # where the numbers 0, 1, 2, 3, 4 are appended into the DLL
#'
#' ### immutable methods
#'
#' # show
#' dll$show()
#'
#' # elem_at
#' dll$elem_at(1)
#'
#' # toList
#' tmp <- dll$toList
#'
#' ### mutable methods
#'
#' # insert_at
#' dll$insert_at(1, -1)
#' dll$insert_at(dll$size+1, "end")
#'
#' # remove_at
#' for(iter in 1:dll$size) dll$remove_at(1)
#'
#' @export
RDLL <- R6Class("RDLL", inherit = RDeque, portable = FALSE, class = FALSE)

RDLL$set("public", "initialize", function(..., collapse=NULL){
  super$initialize(..., collapse=collapse)
})

RDLL$set("public", "show", function(callback=function(val){print(val)}, ...){
  if(.len == 0) return(NULL)

  for(iter in 1:.len)
    do.call(callback, c(list(.elem[[.front+iter]]), list(...)))
  return(invisible(NULL))
})

RDLL$set("public", "elem_at", function(index){
  index = as.integer(index)[1]
  if(index < 1 || index > .len) return(NULL)
  return(.elem[[.front+index]])
})

RDLL$set("public", "insert_at", function(index, val){
  index = as.integer(index)[1]

  if(index < 1 || index > .len+1){
    return(FALSE)
  }else if(index == 1){
    appendleft(val); return(TRUE)
  }else if(index == .len+1){
    append(val); return(TRUE)
  }else{
    .elem <<- c(.elem[(.front+1):(.front+index-1)], list(val), .elem[(.front+index):(.front+.len)])
    .front <<- 0; .len <<- .len+1; return(TRUE)
  }
})

RDLL$set("public", "remove_at", function(index){
  index = as.integer(index)[1]

  if(index < 1 || index > .len){
    return(NULL)
  }else if(index == 1){
    return(popleft())
  }else if(index == .len){
    return(pop())
  }else{
    ret <- .elem[[.front+index]]
    .elem[[.front+index]] <<- NULL
    .len <<- .len-1
    return(ret)
  }
})
