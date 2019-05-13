##########################################################################################
# Double Linked list
##########################################################################################

#' The RDLL reference class
#'
#' The RDLL reference class implements the data structure doubly linked list (DLL).
#'
#' A doubly linked list is an ordered list of items or nodes.
#' Each node in the DLL has three fields:
#' val storing the value of the node, prev pointing to the previous node and next pointing to the next node.
#'
#' The DLL is a powerful sequantial data structure in the sense that
#' the data structures stack, queue, deque, set and dictionary can be implemented based on it.
#' The \code{RDLL} class has all the methods that \code{\link{RDeque}} has.
#'
#'
#' However, in this package, the corresponding \code{\link{RStack}}, \code{\link{RQueue}} and \code{\link{RDeque}}
#' are implemented separately, because they do not need all the features of the DLL.
#'
#' The DLL is much more friendly and flexible as it offers more useful methods to help the user get access to its nodes
#' than \code{\link{RStack}}, \code{\link{RQueue}} and \code{\link{RDeque}}.
#' See below its immutable methods and mutable methods.
#'
#' It is worth noting that the classes \code{\link{RSet}} and \code{\link{RDict}} inherit the RDLL class,
#' and therefor they have all the methods that the RDLL has.
#'
#' The elements in the DLL are not necessarily to be of the same type,
#' and they can even be of function type.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the DLL data structure, see \href{https://en.wikipedia.org/wiki/Doubly_linked_list}{DLL at Wikipedia}.
#'
#' @seealso \link{RDeque}, \link{RSet}, \link{RDict}, and \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable methods do not change the nodes of the instance.
#'
#' \describe{
#' \item{\code{show(callback=function(val){print(val)}, ...)}}{
#' The \code{show} method takes a funtion input (argument \code{callback})
#' specifying how to handle the value of each node in the DLL.
#' It also takes \code{...} as the additional arguments for the \code{callback} function if any.
#' By default, the \code{show} method prints the nodes by using the \code{print} function.
#'
#' \code{callback=function(val){print(val)}}
#'
#' provided that \code{val} is "printable".
#'
#' You can see that \code{show} is powerful as it makes it possible to freely manipulate the nodes.
#' For example, you can define
#'
#' \code{func <- function(val, arg1, arg2){ do something here on val by using arg1 and arg2 }}
#'
#' and then
#'
#' \code{instance$show(func, arg1, arg2)}
#'
#' And you can also store the node values by using instances of reference classes.
#'
#' \code{func <- function(val, queue){ queue$enqueue(val) }}
#'
#' where \code{queue} is an instance of \code{RQueue}, and then
#'
#' \code{queue <- RQueue$new()}
#'
#' \code{instance$show(func, queue)}
#' }
#'
#' \item{\code{toList}}{
#' This is an active method which returns the node values as a list (a copy).
#' Note that you do not need to write the parenthesis.
#' }
#'
#' \item{\code{elem_at(index)}}{
#' It returns the value (a copy) of the node at position \code{index} (a positive integer).
#' \code{index} must be a scalar, and if it is a vector of more than one element,
#' only the first element will be considered.
#' If the value of \code{index} is out of the scope of the instance,
#' a \code{NULL} will be returned.
#' }
#'
#' \item{\code{is_empty}}{
#' This is an active method which returns a boolean showing if the DLL is empty.
#' Note that you do not need to write the parenthesis.
#' }
#'
#' \item{\code{peekleft}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' \item{\code{peek}}{
#' See \code{\link{RDeque}}.
#' }
#'
#' }
#'
#' @section Mutable Methods:
#'
#' The mutable methods changes the nodes of the instance.
#'
#' \describe{
#'
#' \item{\code{insert_at(index, val)}}{
#' This function inserts a new node with the value \code{val} at position \code{index}.
#' It returns \code{TRUE} if the insertion is successful,
#' and \code{FALSE} if the \code{index} is out of the scope.
#' It will push all the nodes at and after \code{index} rightward.
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
#' This function removes the node at position \code{index} and returns the node's value.
#' It returns \code{NULL} if the \code{index} is out of the scope.
#' It will connect the two nodes at both sides of the node to be removed.
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
#' # the previous RDLL instance will be removed by running the following
#' # and the memory allocated for that one will be cleared,
#' # as now, the variable dll points to another instance of the class.
#' dll <- RDLL$new(0, 1, 2, collapse=list(3, 4))
#' # the following sentence is equivalent to the above
#' dll <- RDLL$new(0, 1, 2, 3, 4)
#' # where the numbers 0, 1, 2, 3, 4 are appended into the DLL
#'
#' ### immutable methods
#'
#' # is_empty
#' dll$is_empty
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
RDLL <- R6Class("RDLL", portable = FALSE, class = FALSE)

RDLL$set("private", ".head", NULL)

RDLL$set("private", ".tail", NULL)

RDLL$set("private", ".len", 0)

RDLL$set("active", "size", function(){ return(.len) })

RDLL$set("public", "initialize", function(..., collapse=NULL){
  items = c(list(...), as.list(collapse))
  .len <<- length(items)
  if(.len == 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
  }else if(.len > 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
    for(iter in 2:.len){
      .tail$setNext(RNode$new(items[[iter]]))
      .tail$Next$setPrev(.tail)
      .tail <<- .tail$Next
    }
  }else{}
})

RDLL$set("public", "show", function(callback=function(val){print(val)}, ...){
  current <- .head
  while(!is.null(current)){
    do.call(callback, c(list(current$Val), list(...)))
    current <- current$Next
  }
  return(invisible(NULL))
})

RDLL$set("active", "toList", function(){
  ret = list(); length(ret) = .len
  current <- .head; iter = 1
  while(!is.null(current)){
    ret[[iter]] <- current$Val
    current <- current$Next
    iter <- iter+1
  }
  return(ret)
})

RDLL$set("public", "elem_at", function(index){
  index = as.integer(index)[1]
  if(index < 1 || index > .len) return(NULL)

  step1 = index - 1; step2 = .len - index
  if(step1<=step2){
    current <- .head
    while(step1 > 0){
      current <- current$Next; step1 <- step1-1
    }
  }else{
    current <- .tail
    while(step2 > 0){
      current <- current$Prev; step2 <- step2-1
    }
  }
  return(current$Val)
})

RDLL$set("active", "is_empty", function(){
  return(.len == 0)
})

RDLL$set("active", "peek", function(){
  if(.len == 0) return(NULL)
  return(.tail$Val)
})

RDLL$set("active", "peekleft", function(){
  if(.len == 0) return(NULL)
  return(.head$Val)
})

RDLL$set("public", "insert_at", function(index, val){
  index = as.integer(index)[1]
  if(index < 1 || index > .len+1) return(FALSE)

  target <- RNode$new(val)

  if(index == .len+1){
    if(is.null(.tail)){
      .head <<- target
    }else{
      .tail$setNext(target)
      target$setPrev(.tail)
    }
    .tail <<- target
  }else{
    step1 = index - 1; step2 = .len - index
    if(step1<=step2){
      current <- .head
      while(step1 > 0){
        current <- current$Next; step1 <- step1-1
      }
    }else{
      current <- .tail
      while(step2 > 0){
        current <- current$Prev; step2 <- step2-1
      }
    }

    if(is.null(current$Prev)){
      .head <<- target
    }else{
      current$Prev$setNext(target)
      target$setPrev(current$Prev)
    }

    target$setNext(current)
    current$setPrev(target)
  }

  .len <<- .len+1
  return(TRUE)
})

RDLL$set("public", "remove_at", function(index){
  index = as.integer(index)[1]
  if(index < 1 || index > .len) return(NULL)

  step1 = index - 1; step2 = .len - index
  if(step1<=step2){
    current <- .head
    while(step1 > 0){
      current <- current$Next; step1 <- step1-1
    }
  }else{
    current <- .tail
    while(step2 > 0){
      current <- current$Prev; step2 <- step2-1
    }
  }

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

  .len <<- .len-1
  return(current$Val)
})

RDLL$set("public", "append", function(..., collapse=NULL){
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

RDLL$set("public", "appendleft", function(..., collapse=NULL){
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

RDLL$set("public", "pop", function(){
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

RDLL$set("public", "popleft", function(){
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

