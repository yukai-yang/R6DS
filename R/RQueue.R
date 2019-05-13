##########################################################################################
# Queue
##########################################################################################

#' The RQueue reference class
#'
#' The RQueue reference class implements the data structure queue.
#'
#' A queue is an ordered list of items following the First-In-First-Out (FIFO) principle.
#' The \code{enqueue} method takes elements and enqueue them into the queue,
#' while the \code{dequeue} method returns and removes the earliest enqueued element in the queue.
#'
#' The elements in the queue are not necessarily to be of the same type,
#' and they can even be of function type.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the queue data structure, see \href{https://en.wikipedia.org/wiki/Queue_(abstract_data_type)}{Queue at Wikipedia}.
#'
#' @seealso \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Mutable Methods:
#'
#' The mutable methods changes the nodes of the instance.
#'
#' \describe{
#'
#' \item{\code{enqueue(..., collapse=NULL)}}{
#' The \code{enqueue} method creates nodes containing the values in \code{...} and \code{collapse},
#' and push them into the deque from the right,
#' which is equivalent to the \code{push} in \code{\link{RStack}}.
#' }
#'
#' \item{\code{dequeue()}}{
#' The \code{dequeue} method returns and removes the leftmost element in the deque.
#' It returns \code{NULL} if the queue is empty.
#' }
#'
#' }
#'
#' @keywords RQueue
#'
#' @examples
#'
#' ### create a new instance
#'
#' # to create a new instance of the class
#' queue <- RQueue$new()
#'
#' # the previous RQueue instance will be removed by running the following
#' # and the memory allocated for that one will be cleared,
#' # as now, the variable queue points to another instance of the class.
#' queue <- RQueue$new(0, 1, 2, collapse=list(3, 4))
#' # the following sentence is equivalent to the above
#' queue <- RQueue$new(0, 1, 2, 3, 4)
#' # where the numbers 0, 1, 2, 3, 4 are enqueued into the queue
#'
#' ### enqueue elements
#'
#' # it can be one single element
#' queue$enqueue(5)
#' # it can be several elements separated by commas
#' # note the whole list will be one element of the queue
#' # because it is not passed through the collapse argument
#' queue$enqueue(list(a=10,b=20), "Hello world!")
#' # the collapse argument takes a list whose elements will be collapsed
#' # but the elements' names will not be saved
#' queue$enqueue(collapse = list(x=100,y=200))
#' # they can be used together
#' queue$enqueue("hurrah", collapse = list("RQueue",300))
#'
#' ### dequeue an element
#'
#' # dequeue only one element at a time
#' val <- queue$dequeue()
#' # then we keep dequeuing!
#' while(!is.null(val)) val <- queue$dequeue()
#'
#' @export
RQueue <- R6Class("RQueue", portable = FALSE, class = FALSE)

RQueue$set("private", ".head", NULL)

RQueue$set("private", ".tail", NULL)

RQueue$set("private", ".len", 0)

RQueue$set("active", "size", function(){ return(.len) })

RQueue$set("public", "initialize", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  .len <<- length(items)
  if(.len == 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
  }
  else if(.len > 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
    for(iter in 2:.len){
      .tail$setNext(RNode$new(items[[iter]]))
      .tail <<- .tail$Next
    }
  }else{}
})

RQueue$set("public", "enqueue", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  if(.len > 0){
    .len <<- .len+length(items)
    for(item in items){
      .tail$setNext(RNode$new(item))
      .tail <<- .tail$Next
    }
  }else{
    .len <<- length(items)
    if(.len == 1){
      .head <<- RNode$new(items[[1]])
      .tail <<- .head
    }
    else if(.len > 1){
      .head <<- RNode$new(items[[1]])
      .tail <<- .head
      for(iter in 2:.len){
        .tail$setNext(RNode$new(items[[iter]]))
        .tail <<- .tail$Next
      }
    }else{}
  }
})

RQueue$set("public", "dequeue", function(){
  if(.len == 0) return(NULL)
  current <- .head
  .head <<- .head$Next
  .len <<- .len-1
  if(.len == 0) .tail <<- NULL
  return(current$Val)
})
