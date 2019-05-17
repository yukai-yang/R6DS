##########################################################################################
# Queue
##########################################################################################

#' The RQueue reference class
#'
#' The RQueue reference class implements the data structure queue.
#'
#' A queue is an ordered list of elements following the First-In-First-Out (FIFO) principle.
#' The \code{enqueue} method takes elements and add them to the rear terminal position (right) of the queue,
#' while the \code{dequeue} method returns and removes the element in the queue from the front terminal position (left).
#'
#' The elements in the queue are not necessarily to be of the same type,
#' and they can be any R objects.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the queue data structure, see \href{https://en.wikipedia.org/wiki/Queue_(abstract_data_type)}{Queue at Wikipedia}.
#'
#' @seealso \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable method does not change the instance.
#'
#' \describe{
#'
#' \item{\code{peekleft()}}{
#' This method returns the leftmost (front) element in the queue.
#' It returns \code{NULL} if the queue is empty.
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
#' \item{\code{enqueue(..., collapse=NULL)}}{
#' The \code{enqueue} method enqueues the elements in \code{...} and \code{collapse} into the queue
#' (to the right or rear).
#'
#' Note that you can input multiple elements.
#' }
#'
#' \item{\code{dequeue()}}{
#' The \code{dequeue} method dequeues (returns and removes) one element (the leftmost or front) fron the queue.
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
#' # the previous RQueue instance will be removed if you run
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

RQueue$set("private", ".elem", list())

# the position before the first node
RQueue$set("private", ".front", 0)

RQueue$set("private", ".len", 0)

RQueue$set("active", "size", function(){ return(.len) })

RQueue$set("public", "initialize", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  .len <<- length(items)

  iter <- 1
  for(item in items){
    .elem[[iter]] <<- item
    iter <- iter+1
  }
})

RQueue$set("active", "toList", function(){
  if(.len == 0) return(list())
  return(.elem[(.front+1):(.front+.len)])
})

RQueue$set("public", "is_empty", function(){
  return(.len == 0)
})

RQueue$set("public", "peekleft", function(){
  if(.len == 0) return(NULL)
  return(.elem[[.front+1]])
})

RQueue$set("public", "enqueue", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))

  iter <- .front+.len+1
  for(item in items){
    .elem[[iter]] <<- item
    iter <- iter+1
  }
  .len <<- .len+length(items)

  return(invisible(NULL))
})

RQueue$set("public", "dequeue", function(){
  if(.len == 0) return(NULL)
  .front <<- .front+1
  .len <<- .len-1
  return(.elem[[.front]])
})

RQueue$set("public", "release", function(){
  if(.len > 0){ .elem <<- .elem[(.front+1):(.front+.len)]
  }else .elem <<- list()
  .front <<- 0
  return(invisible(NULL))
})
