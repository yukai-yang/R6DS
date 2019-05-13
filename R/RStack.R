##########################################################################################
# Stack
##########################################################################################

#' The RStack reference class
#'
#' The RStack reference class implements the data structure stack.
#'
#' A stack is an ordered list of items following the Last-In-First-Out (LIFO) principle.
#' The \code{push} method takes elements and push them into the stack,
#' while the \code{pop} method returns and removes the last "pushed" element in the stack.
#'
#' The elements in the stack are not necessarily to be of the same type,
#' and they can even be of function type.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the stack data structure, see \href{https://en.wikipedia.org/wiki/Stack_(abstract_data_type)}{Stack at Wikipedia}.
#'
#' @seealso \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Mutable Methods:
#'
#' The mutable methods changes the nodes of the instance.
#'
#' \describe{
#'
#' \item{\code{push(..., collapse=NULL)}}{
#' The \code{push} method creates nodes containing the values in \code{...} and \code{collapse},
#' and push them into the stach on the tail,
#' which is equivalent to the \code{enqueue} in \code{\link{RQueue}}.
#' }
#'
#' \item{\code{pop()}}{
#' The \code{pop} method returns and removes the last pushed element in the stack.
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
#' # the previous RStack instance will be removed by running the following
#' # and the memory allocated for that one will be cleared,
#' # as now stack has been pointed to another instance of the class.
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

RStack$set("private", ".tail", NULL)

RStack$set("private", ".len", 0)

RStack$set("active", "size", function(){ return(.len) })

RStack$set("public", "initialize", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  .len <<- length(items)
  for(item in items){
    current <- RNode$new(item)
    current$setPrev(.tail)
    .tail <<- current
  }
})

RStack$set("public", "push", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  .len <<- .len+length(items)
  for(item in items){
    current <- RNode$new(item)
    current$setPrev(.tail)
    .tail <<- current
  }
})

RStack$set("public", "pop", function(){
  if(.len == 0) return(NULL)
  current <- .tail
  .tail <<- .tail$Prev
  .len <<- .len-1
  return(current$Val)
})
