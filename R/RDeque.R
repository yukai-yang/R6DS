##########################################################################################
# Double-Ended Queue
##########################################################################################

#' The RDeque reference class
#'
#' The RDeque reference class implements the data structure double-ended queue (deque).
#'
#' A deque is an ordered list of items combining both the stack and the queue.
#' Each node in the DLL has three fields:
#' val storing the value of the node, prev pointing to the previous node and next pointing to the next node.
#'
#' The deque is slightly more powerful than stack and queue, as it can append elements from left or the head.
#' See \code{\link{RStack}} and \code{\link{RQueue}} for the introductions of the two classes.
#' It has a generalized version: the doubly linked list (DLL), see \code{\link{RDLL}}.
#'
#' Different from the \link{RStack} and \link{RQueue} classes, you can check the leftmost and rightmost elements
#' by using \code{peekleft} and \code{peek} methods.
#' Note that they are both active methods and do not change the deque but just return the elements.
#'
#' The elements in the deque are not necessarily to be of the same type,
#' and they can even be of function type.
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
#' The immutable methods do not change the nodes of the instance.
#'
#' \describe{
#' \item{\code{peekleft}}{
#' This function is an active method which returns the value of the leftmost node of the deque.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' \item{\code{peek}}{
#' This function is an active method which returns the value of the rightmost node of the deque.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' }
#'
#' @section Mutable Methods:
#'
#' The mutable methods changes the nodes of the instance.
#'
#' \describe{
#' \item{\code{appendleft(..., collapse=NULL)}}{
#' The \code{appendleft} method creates nodes containing the values in \code{...} and \code{collapse},
#' and the push them into the deque from the left.
#' Note that if you push elements in this manner:
#'
#' \code{instance$appendleft(elem1, elem2, elem3)}
#'
#' The order of them inside the deque will be
#'
#' \code{elem3, elem2, elem1, ...}
#'
#' from left to right, and elem3 will be the new head of the deque.
#' }
#'
#' \item{\code{append(..., collapse=NULL)}}{
#' The \code{append} method creates nodes containing the values in \code{...} and \code{collapse},
#' and push them into the deque from the right,
#' which is equivalent to the \code{push} in \code{\link{RStack}} and \code{enqueue} in \code{\link{RQueue}}.
#' }
#'
#' \item{\code{popleft()}}{
#' The \code{popleft} method returns and removes the leftmost element in the deque,
#' which is equivalent to the \code{dequeue} in \code{\link{RQueue}}.
#' It returns \code{NULL} if the deque is empty.
#' }
#'
#' \item{\code{pop()}}{
#' The \code{pop} method returns and removes the rightmost element in the deque,
#' which is equivalent to the \code{pop} in \code{\link{RStack}}.
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
#' # the previous RDeque instance will be removed by running the following
#' # and the memory allocated for that one will be cleared,
#' # as now, the variable deque points to another instance of the class.
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
#' deque$peekleft
#' # "string1"
#' deque$peek
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

RDeque$set("private", ".head", NULL)

RDeque$set("private", ".tail", NULL)

RDeque$set("private", ".len", 0)

RDeque$set("active", "size", function(){ return(.len) })

RDeque$set("public", "initialize", function(..., collapse=NULL){
  items = c(list(...), as.list(collapse))
  .len <<- length(items)
  if(.len == 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
  }
  if(.len > 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
    for(iter in 2:.len){
      .tail$setNext(RNode$new(items[[iter]]))
      .tail$Next$setPrev(.tail)
      .tail <<- .tail$Next
    }
  }
})

RDeque$set("public", "append", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  if(.len > 0){
    .len <<- .len+length(items)
    for(item in items){
      .tail$setNext(RNode$new(item))
      .tail$Next$setPrev(.tail)
      .tail <<- .tail$Next
    }
  }else{
    .len <<- length(items)
    if(.len == 1){
      .head <<- RNode$new(items[[1]])
      .tail <<- .head
    }
    if(.len > 1){
      .head <<- RNode$new(items[[1]])
      .tail <<- .head
      for(iter in 2:.len){
        .tail$setNext(RNode$new(items[[iter]]))
        .tail$Next$setPrev(.tail)
        .tail <<- .tail$Next
      }
    }
  }
  return(invisible(NULL))
})

RDeque$set("public", "appendleft", function(..., collapse=NULL){
  items <- c(list(...), as.list(collapse))
  if(.len > 0){
    .len <<- .len+length(items)
    for(item in items){
      .head$setPrev(RNode$new(item))
      .head$Prev$setNext(.head)
      .head <<- .head$Prev
    }
  }else{
    .len <<- length(items)
    if(.len == 1){
      .head <<- RNode$new(items[[1]])
      .tail <<- .head
    }
    if(.len > 1){
      .head <<- RNode$new(items[[1]])
      .tail <<- .head
      for(iter in 2:.len){
        .head$setPrev(RNode$new(item))
        .head$Prev$setNext(.head)
        .head <<- .head$Prev
      }
    }
  }
  return(invisible(NULL))
})

RDeque$set("public", "pop", function(){
  if(.len == 0){
    return(NULL)
  }else if(.len == 1){
    current <- .tail
    .tail <<- NULL
    .head <<- NULL
    .len <<- 0
    return(current$Val)
  }else{
    current <- .tail
    .tail <<- .tail$Prev
    .tail$setNext(NULL)
    current$setPrev(NULL)
    .len <<- .len-1
    return(current$Val)
  }
})

RDeque$set("public", "popleft", function(){
  if(.len == 0){
    return(NULL)
  }else if(.len == 1){
    current <- .head
    .head <<- NULL
    .tail <<- NULL
    .len <<- 0
    return(current$Val)
  }else{
    current <- .head
    .head <<- .head$Next
    .head$setPrev(NULL)
    current$setNext(NULL)
    .len <<- .len-1
    return(current$Val)
  }
})

RDeque$set("active", "peek", function(){
  if(.len == 0) return(NULL)
  return(.tail$Val)
})

RDeque$set("active", "peekleft", function(){
  if(.len == 0) return(NULL)
  return(.head$Val)
})
