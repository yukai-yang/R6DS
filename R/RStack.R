##########################################################################################
# Stack
##########################################################################################

#' The RStack reference class
#'
#' The RStack reference class implements the data structure stack.
#'
#' A stack is an ordered list of elements following the Last-In-First-Out (LIFO) principle.
#' The \code{push} method takes elements and add them to the top position (right) of the stack,
#' while the \code{pop} method returns and removes the last "pushed" (top or rightmost) element in the stack.
#'
#' The elements in the stack are not necessarily to be of the same type,
#' and they can be any R objects.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the stack data structure, see \href{https://en.wikipedia.org/wiki/Stack_(abstract_data_type)}{Stack at Wikipedia}.
#'
#' @seealso \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable method does not change the instance.
#'
#' \describe{
#'
#' \item{\code{peek()}}{
#' This method returns the last pushed (top or rightmost) element in the stack.
#' It returns \code{NULL} if the stack is empty.
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
#' \item{\code{push(..., collapse=NULL)}}{
#' The \code{push} method pushes the elements in \code{...} and \code{collapse} into the stack
#' (to the top or right).
#'
#' Note that you can input multiple elements.
#' }
#'
#' \item{\code{pop()}}{
#' The \code{pop} method pops (returns and removes) the last pushed (rightmost) element in the stack.
#' It returns \code{NULL} if the stack is empty.
#' }
#'
#' }
#'
#' @keywords RStack
#'
#' @examples
#'
#' ### create a new instance
#'
#' # to create a new instance of the class
#' stack <- RStack$new()
#'
#' # the previous RStack instance will be removed if you run
#' stack <- RStack$new(0, 1, 2, collapse=list(3, 4))
#' # the following sentence is equivalent to the above
#' stack <- RStack$new(0, 1, 2, 3, 4)
#' # where the numbers 0, 1, 2, 3, 4 are pushed into the stack
#'
#' ### push elements
#'
#' # it can be one single element
#' stack$push(5)
#' # it can be several elements separated by commas
#' # note the whole list will be one element of the stack
#' # because it is not passed through the collapse argument
#' stack$push(list(a=10,b=20), "Hello world!")
#' # the collapse argument takes a list whose elements will be collapsed
#' # but the elements' names will not be saved
#' stack$push(collapse = list(x=100,y=200))
#' # they can be used together
#' stack$push("hurrah", collapse = list("RStack",300))
#'
#' ### pop an element
#'
#' # pop only one element at a time
#' val <- stack$pop()
#' # then we keep poping!
#' while(!is.null(val)) val <- stack$pop()
#'
#' @export
RStack <- R6Class("RStack", portable = FALSE, class = FALSE)

RStack$set("private", ".elem", list())

RStack$set("private", ".len", 0)

RStack$set("active", "size", function(){ return(.len) })

RStack$set("public", "initialize", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  .len <<- length(items)
  iter <- 1
  for(item in items){
    .elem[[iter]] <<- item
    iter <- iter+1
  }
})

RStack$set("active", "toList", function(){
  if(.len == 0) return(list())
  return(.elem[1:.len])
})

RStack$set("public", "is_empty", function(){
  return(.len == 0)
})

RStack$set("public", "peek", function(){
  if(.len == 0) return(NULL)
  return(.elem[[.len]])
})

RStack$set("public", "push", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  iter <- .len+1
  for(item in items){
    .elem[[iter]] <<- item
    iter <- iter+1
  }
  .len <<- .len+length(items)
  return(invisible(NULL))
})

RStack$set("public", "pop", function(){
  if(.len == 0) return(NULL)
  .len <<- .len-1
  return(.elem[[.len+1]])
})

RStack$set("public", "release", function(){
  if(.len > 0){ .elem <<- .elem[1:.len]
  }else .elem <<- list()
  return(invisible(NULL))
})
