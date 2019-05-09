#################################################################################
## utility functions
#################################################################################

vnum = "1.0.0"
packname = "(Red DS)"

# simple cat
cat0 <- function(...)
{
  words = list(...)
  for(tmp in words) cat(tmp)
  cat("\n")
}

#' Show the version number of some information.
#'
#' This function shows the version number and some information of the package.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords utils
#'
#' @export
version <- function(){
  cat0("R6DS version ", vnum, " ",packname)
}


##########################################################################################
# Node
##########################################################################################

#' The node reference class
#'
#' The node reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RNode <- R6Class("RNode", portable = FALSE, class = FALSE)

RNode$set("private", ".val", NULL)

RNode$set("private", ".next", NULL)

RNode$set("private", ".prev", NULL)

RNode$set("public", "initialize", function(val){ .val <<- val })

RNode$set("public", "setNext", function(node){ .next <<- node })

RNode$set("public", "setPrev", function(node){ .prev <<- node })

RNode$set("active", "Val", function(){ return(.val) })

RNode$set("active", "Prev", function(){ return(.prev) })

RNode$set("active", "Next", function(){ return(.next) })

#RNode$set("public", "finalize", function(){ message("node deleted!") })


##########################################################################################
# Linked list
##########################################################################################

#' The linked list reference class
#'
#' The linked list reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RLinkedList <- R6Class("RLinkedList", portable = FALSE, class = FALSE)

RLinkedList$set("private", ".head", NULL)

RLinkedList$set("private", ".tail", NULL)

RLinkedList$set("private", ".len", 0)

RLinkedList$set("private", ".iter", NULL)

RLinkedList$set("active", "size", function(){ return(.len) })

RLinkedList$set("public", "initialize", function(..., collapse=NULL){
  items = c(list(...), as.list(collapse))
  .len <<- length(items)
  if(.len == 1){
    .head <<- RNode$new(items[[1]])
    .tail <<- .head
  }
  if(.len > 1){
    .head <<- RNode$new(items[[1]])
    current <- .head
    for(iter in 2:.len){
      current$setNext(RNode$new(items[[iter]]))
      current$Next$setPrev(current)
      current <- current$Next
    }
    .tail <<- current
  }
})

RLinkedList$set("public", "show", function(depict=function(val){cat(val,"\n",sep="")}){
  current <- .head
  while(!is.null(current)){
    depict(current$Val)
    current <- current$Next
  }
  return(invisible(self))
})

RLinkedList$set("active", "toList", function(){
  ret = list(); length(ret) = .len
  current <- .head; iter = 1
  while(!is.null(current)){
    ret[[iter]] <- current$Val
    current <- current$Next
    iter <- iter+1
  }
  return(ret)
})

RLinkedList$set("public", "ElemAt", function(index){
  index = as.integer(index)
  stopifnot(index >= 1, index <= .len)

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

RLinkedList$set("public", "Insert", function(index, val){
  index = as.integer(index)
  stopifnot(index >= 1, index <= .len+1)

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
})

RLinkedList$set("public", "Append", function(val){
  Insert(.len+1, val)
})

RLinkedList$set("active", "isEmpty", function(){
  return(.len == 0)
})

RLinkedList$set("active", "initIterator", function(){ .iter <<- .head })

RLinkedList$set("active", "hasNext", function(){ return(!is.null(.iter)) })

RLinkedList$set("active", "getNext", function(){
  current <- .iter; .iter <<- .iter$Next
  return(current$Val)
})

RLinkedList$set("public", "RemoveAt", function(index){
  index = as.integer(index)
  stopifnot(index >= 1, index <= .len)

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


##########################################################################################
# Stack
##########################################################################################

#' The stack reference class
#'
#' The stack reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RStack <- R6Class("RStack", inherit = RLinkedList, portable = FALSE, class = FALSE)

RStack$set("public", "initialize", function(..., collapse=NULL){ super$initialize(...,collapse=collapse) })

RStack$set("public", "push", function(val){ super$Append(val) })

RStack$set("public", "pop", function(){ return(super$RemoveAt(.len)) })


##########################################################################################
# Queue
##########################################################################################

#' The queue reference class
#'
#' The queue reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RQueue <- R6Class("RQueue", inherit = RLinkedList, portable = FALSE, class = FALSE)

RQueue$set("public", "initialize", function(..., collapse=NULL){ super$initialize(...,collapse=collapse) })

RQueue$set("public", "Enqueue", function(val){ super$Append(val) })

RQueue$set("public", "Dequeue", function(){ return(super$RemoveAt(1)) })


##########################################################################################
# Set
# a set is a linked list equipped with equal function
##########################################################################################

#' The set reference class
#'
#' The set reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RSet <- R6Class("RSet", inherit = RLinkedList, portable = FALSE, class = FALSE)

RSet$set("private", "equal", NULL)

RSet$set("public", "initialize", function(equal, ..., collapse=NULL){
  equal <<- equal
  items = c(list(...), as.list(collapse))

  for(item in items) Add(item)
})

RSet$set("public", "Has", function(val){
  current <- .head
  while(!is.null(current)){
    if(equal(current$Val, val)) return(TRUE)
    current <- current$Next
  }
  return(FALSE)
})

RSet$set("public", "Add", function(val){
  if(!Has(val)){ super$Append(val); return(TRUE)}
  return(FALSE)
})

RSet$set("public", "Delete", function(val){
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


##########################################################################################
# Dictionary
# a dictionary is a linked list whose elements have keys and values
##########################################################################################

#' The dictionary reference class
#'
#' The dictionary reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RDict <- R6Class("RDict", inherit = RLinkedList, portable = FALSE, class = FALSE)

RDict$set("public", "initialize", function(..., collapse=NULL){
  items = c(list(...), as.list(collapse))
  keys = names(items)

  for(iter in seq_along(items)){
    if(keys[iter]=="") next
    Add(key=keys[iter], val=items[[iter]])
  }
})

RDict$set("public", "Has", function(key){
  key = gsub('"',"", deparse(substitute(x)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))

  current <- .head
  while(!is.null(current)){
    if(current$Val$key == key) return(TRUE)
    current <- current$Next
  }
  return(FALSE)
})

RDict$set("public", "Get", function(key){
  key = gsub('"',"", deparse(substitute(x)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))

  current <- .head
  while(!is.null(current)){
    if(current$Val$key == key) return(current$Val$val)
    current <- current$Next
  }
  return(NULL)
})

RDict$set("public", "Delete", function(key){
  key = gsub('"',"", deparse(substitute(x)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))

  current <- .head
  while(!is.null(current)){
    if(current$Val$key == key){

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

RDict$set("public", "Add", function(..., key="", val=NULL){
  items = list(...); keys = names(items)

  for(iter in seq_along(items)){
    if(keys[iter] == "") next
    if(!Has(keys[iter])) super$Append(list(key=keys[iter], val=items[[iter]]))
  }

  tmp = try(is.character(key), silent=T)
  if(class(tmp) == "try-error") key = deparse(substitute(key))
  if(key != "") if(!Has(key)) super$Append(list(key=key, val=val))
})

RDict$set("public", "show", function(){
  current <- .head
  while(!is.null(current)){
    cat("key:",current$Val$key,"\n")
    cat("val:",current$Val$val,"\n")
    current <- current$Next
  }
  return(invisible(self))
})

RDict$set("active", "keys", function(){
  ret <- rep("", .len); iter <- 1
  current <- .head
  while(!is.null(current)){
    ret[iter] <- current$Val$key
    current <- current$Next
    iter <- iter+1
  }
  return(ret)
})

RDict$set("active", "values", function(){
  ret <- list(); length(ret) <- .len; iter <- 1
  current <- .head
  while(!is.null(current)){
    ret[[iter]] <- current$Val$val
    current <- current$Next
    iter <- iter+1
  }
  return(ret)
})


##########################################################################################
# Binary Search Tree (BST)
##########################################################################################

# a lessthan function should be predefined
# We use RNode as the node of the BST
# .prev is .left, and .next is .right

#' The binary search tree (BST) reference class
#'
#' The binary search tree (BST) reference class gives
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#' @keywords class
#'
#' @export
RBST <- R6Class("RBST", portable = FALSE, class = FALSE)

RBST$set("private", ".root", NULL)

RBST$set("private", ".len", 0)

RBST$set("private", "lessthan", NULL)

RBST$set("private", "equal", NULL)

RBST$set("active", "size", function(){ return(.len) })

# the lessthan function defines < between values
# the equal function defines == between values
RBST$set("public", "initialize", function(lessthan, equal, ..., collapse=NULL){
  lessthan <<- lessthan; equal <<- equal
  items = list(...); items = c(items, as.list(collapse))

  for(item in items) insertBST(item)
})

RBST$set("public", "traverseBST", function(mode="in-order",callback=function(val){cat(" ->",val," ")}){
  if(mode == "in-order"){
    traverse_in_order(node=.root, callback=callback)
  }

  if(mode == "pre-order"){
    traverse_pre_order(node=.root, callback=callback)
  }

  if(mode == "post-order"){
    traverse_post_order(node=.root, callback=callback)
  }

  return(NULL)
})

# callback is a function about to treat the val in each node
RBST$set("private", "traverse_in_order", function(node, callback){
  if(is.null(node)) return()

  traverse_in_order(node=node$Prev, callback=callback)
  callback(node$Val)
  traverse_in_order(node=node$Next, callback=callback)

})

# callback is a function about to treat the val in each node
RBST$set("private", "traverse_pre_order", function(node, callback){
  if(is.null(node)) return(NULL)

  callback(node$Val)
  traverse_pre_order(node=node$Prev, callback=callback)
  traverse_pre_order(node=node$Next, callback=callback)

  return(NULL)
})

# callback is a function about to treat the val in each node
RBST$set("private", "traverse_post_order", function(node, callback){
  if(is.null(node)) return(NULL)

  traverse_post_order(node=node$Prev, callback=callback)
  traverse_post_order(node=node$Next, callback=callback)
  callback(node$Val)

  return(NULL)
})

# the function find key == val of some node in the subtree from root
# node and (its) parent are references to two nodes in the tree
# node is where the search starts
# parent is just the node's parent if any
RBST$set("private", ".search", function(key, node=NULL, parent=NULL){
  if(is.null(node)) return(list(found=FALSE, ref=parent))

  if(equal(key, node$Val)) return(list(found=TRUE, ref=node, par=parent))

  if(lessthan(key, node$Val)){# go left or prev
    return(.search(key=key, node=node$Prev, parent=node))
  }else{# go right or next
    return(.search(key=key, node=node$Next, parent=node))
  }
})


RBST$set("public", "searchBST", function(val){
  ret = .search(key=val, node=.root)
  if(ret$found){ return(ret$ref$Val)
  }else return(NULL)
})

# insert the node with val into the subtree stem from node
# note that the node should not be NULL
RBST$set("private", "insert", function(val, node){
  if(equal(val, node$Val)) return(FALSE)

  if(lessthan(val, node$Val)){# go left or prev
    if(is.null(node$Prev)){
      node$setPrev(RNode$new(val)); .len <<- .len+1; return(TRUE)
    }
    return(insert(val, node$Prev))
  }else{# go right or next
    if(is.null(node$Next)){
      node$setNext(RNode$new(val)); .len <<- .len+1; return(TRUE)
    }
    return(insert(val, node$Next))
  }
})

RBST$set("public", "insertBST", function(val){
  if(is.null(.root)){
    .root <<- RNode$new(val); .len <<- 1; return(TRUE)
  }
  return(insert(val, .root))
})

RBST$set("private", ".min", function(node){
  if(is.null(node$Prev)) return(node$Val)
  return(.min(node$Prev))
})

RBST$set("active", "min", function(){
  if(is.null(.root)) return(NULL)
  return(.min(.root))
})

RBST$set("private", ".max", function(node){
  if(is.null(node$Next)) return(node$Val)
  return(.max(node$Next))
})

RBST$set("active", "max", function(){
  if(is.null(.root)) return(NULL)
  return(.max(.root))
})

# delete the node == val
RBST$set("public", "deleteBST", function(val){
  ret = .search(key=val, node=.root)

  if(ret$found){
    current <- ret$ref

    if(is.null(current$Prev)){
      # left child empty
      update <- current$Next
    }else{
      # right child empty
      update <- current$Prev

      if(!is.null(current$Next)){
        # if neither children are empty
        if(!is.null(update$Next)){
          tmp <- update
          update <- update$Next
          while(!is.null(update$Next)){
            tmp <- update
            update <- update$Next
          }

          tmp$setNext(update$Prev)
          update$setPrev(current$Prev)
          update$setNext(current$Next)
        }
      }
    }

    # remove current
    if(is.null(ret$par)){
      .root <<- update
    }else{
      parent <- ret$par
      if(lessthan(current$Val, parent$Val)){
        parent$setPrev(update)
      }else{
        parent$setNext(update)
      }
    }

    .len <<- .len-1
  }

  return(ret$found)
})
