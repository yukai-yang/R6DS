##########################################################################################
# Double-Ended Queue
##########################################################################################

#' The RDeque reference class
#'
#' The RDeque reference class implements the data structure double-ended queue (deque).
#'
#' A deque is an ordered list of elements generalizing the queue data structure.
#' One can append and pop (return and remove) elements from both sides
#' (left and right, front and rear) of the deque.
#'
#' The elements in the deque are not necessarily to be of the same type,
#' and they can be any R objects.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the deque data structure, see \href{https://en.wikipedia.org/wiki/Double-ended_queue}{Deque at Wikipedia}.
#'
#' @seealso \link{RStack}, \link{RQueue}, and \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable methods do not change the instance.
#'
#' \describe{
#'
#' \item{\code{peekleft()}}{
#' This method returns the leftmost (front) element of the deque.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' \item{\code{peek()}}{
#' This method returns the rightmost (rear) element of the deque.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' }
#'
#' @section Mutable Methods:
#'
#' The mutable methods change the instance.
#'
#' \describe{
#' \item{\code{appendleft(..., collapse=NULL)}}{
#' The \code{appendleft} method appends the elements in \code{...} and \code{collapse}
#' into the deque to the left (front).
#'
#' Note that if you append elements in this order:
#'
#' \code{instance$appendleft(elem1, elem2, elem3)}
#'
#' The order of them inside the deque will be
#'
#' \code{elem3, elem2, elem1, ...}
#'
#' and \code{elem3} will be the new front of the deque.
#' }
#'
#' \item{\code{append(..., collapse=NULL)}}{
#' The \code{append} method appends the elements in \code{...} and \code{collapse}
#' into the deque to the right (rear).
#' }
#'
#' \item{\code{popleft()}}{
#' The \code{popleft} method returns and removes the leftmost (front) element in the deque.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' \item{\code{pop()}}{
#' The \code{pop} method returns and removes the rightmost (rear) element in the deque.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' }
#'
#' @keywords RDeque
#'
#' @examples
#'
#' ### create a new instance
#'
#' # to create a new instance of the class
#' deque <- RDeque$new()
#'
#' # the previous RDeque instance will be removed if you run
#' deque <- RDeque$new(0, 1, 2, collapse=list(3, 4))
#' # the following sentence is equivalent to the above
#' deque <- RDeque$new(0, 1, 2, 3, 4)
#' # where the numbers 0, 1, 2, 3, 4 are enqueued into the deque
#'
#' ### append and appendleft
#'
#' # it can be one single element
#' deque$append(5)
#' # it can be several elements separated by commas
#' # note the whole list will be one element of the deque
#' # because it is not passed through the collapse argument
#' deque$append(list(a=10,b=20), "Hello world!")
#' # the collapse argument takes a list whose elements will be collapsed
#' # but the elements' names will not be saved
#' deque$append(collapse = list(x=100,y=200))
#' # they can be used together
#' deque$append("hurrah", collapse = list("RDeque",300))
#'
#' # this string will be the new head
#' deque$appendleft("a string")
#' # we can update the head by
#' deque$appendleft("string3","string2","string1")
#' # "string1" will be the leftmost
#'
#' ### peekleft and peek
#' deque$peekleft()
#' # "string1"
#' deque$peek()
#' # 300
#'
#' ### popleft and pop
#'
#' val <- deque$popleft()
#' # "string1"
#' val <- deque$pop()
#' # 300
#'
#' # then we keep dequeuing!
#' while(!is.null(val)) val <- deque$pop()
#'
#' @export
RDeque <- R6Class("RDeque", portable = FALSE, class = FALSE)

RDeque$set("private", ".elem", list())

# the position before the first node
RDeque$set("private", ".front", 0)

RDeque$set("private", ".len", 0)

RDeque$set("active", "size", function(){ return(.len) })

RDeque$set("public", "initialize", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  .len <<- length(items)

  iter <- 1
  for(item in items){
    .elem[[iter]] <<- item
    iter <- iter+1
  }
})

RDeque$set("active", "toList", function(){
  if(.len == 0) return(list())
  return(.elem[(.front+1):(.front+.len)])
})

RDeque$set("public", "is_empty", function(){
  return(.len == 0)
})

RDeque$set("public", "peek", function(){
  if(.len == 0) return(NULL)
  return(.elem[[.front+.len]])
})

RDeque$set("public", "peekleft", function(){
  if(.len == 0) return(NULL)
  return(.elem[[.front+1]])
})

RDeque$set("public", "append", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))

  iter <- .front+.len+1
  for(item in items){
    .elem[[iter]] <<- item
    iter <- iter+1
  }
  .len <<- .len+length(items)

  return(invisible(NULL))
})

RDeque$set("public", "appendleft", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  nitems <- length(items)

  # add buffer
  if(nitems > .front){
    tmp <- list(); ltmp <- max(20, nitems)
    length(tmp) <- ltmp - .front
    .elem <<- c(tmp, .elem)
    .front <<- ltmp
  }

  for(item in items){
    .elem[[.front]] <<- item
    .front <<- .front-1
  }
  .len <<- .len+length(items)

  return(invisible(NULL))
})

RDeque$set("public", "pop", function(){
  if(.len == 0) return(NULL)
  .len <<- .len-1
  return(.elem[[.front+.len+1]])
})

RDeque$set("public", "popleft", function(){
  if(.len == 0) return(NULL)
  .front <<- .front+1
  .len <<- .len-1
  return(.elem[[.front]])
})

RDeque$set("public", "release", function(){
  if(.len > 0){ .elem <<- .elem[(.front+1):(.front+.len)]
  }else .elem <<- list()
  .front <<- 0
  return(invisible(NULL))
})
