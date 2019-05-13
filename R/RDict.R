##########################################################################################
# Dictionary
# a dictionary is a linked list whose elements have keys and values
##########################################################################################

#' The RDict reference class
#'
#' The RDict reference class implements the data structure dictionary.
#'
#' A dictionary is a collection of (key, value) pairs as its elements
#' such that each possible key appears at most once in the collection.
#' The dictionary data structure does not care the order of the elements.
#'
#' The class \code{RDict} inherits the \code{\link{RDLL}} class,
#' and therefor it has all the methods that \code{\link{RDLL}} has.
#'
#' Note that the methods \code{insert_at}, \code{appendleft}, \code{append}
#' in the super class still works without checking if the new element has the key equal to
#' any other keys in the dictionary.
#' Normally they should be depreciated in the \code{RDict} class,
#' but this is not done in the current version of the package.
#' It is strongly recommended that the user should use the \code{add} method
#' to add a new element when using the \code{RDict} class.
#'
#' The keys of the elements in the dictionary must be strings.
#' However, the values of the elements in the dictionary are not necessarily to be of the same type,
#' and they can even be of function type.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the dictionary data structure, see \href{https://en.wikipedia.org/wiki/Associative_array}{Dictionary at Wikipedia}.
#'
#' @seealso \link{RDLL} and \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable methods do not change the elements of the instance.
#'
#' \describe{
#'
#' \item{\code{has(key)}}{
#' The method \code{has} returns a boolean indicating
#' if the dictionary contains the element with the key name \code{"key"}.
#'
#' Both of the following two sentences are equivalent:
#'
#' \code{instance$has("keyname")}
#'
#' \code{instance$has(keyname)}
#' }
#'
#' \item{\code{get(key)}}{
#' The method \code{get} returns the value of the element whose key is \code{"key"}.
#' It returns \code{NULL} if no element is found.
#' }
#'
#' \item{\code{keys}}{
#' The method \code{keys} returns a vector of the keys in the dictionary.
#' }
#'
#' \item{\code{values}}{
#' The method \code{values} returns a list of the values in the dictionary.
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
#' \item{\code{add(..., key="", val=NULL)}}{
#' The method \code{add} adds new elements into the dictionary.
#' It will not add element with the key which exists already in the dictionary.
#'
#' The argument \code{...} stands for any input with the form
#'
#' \code{keyname = value}
#'
#' Therefor, the input can take the form
#'
#' \code{instance$add(key1=1, key2="hello", key3=list(1))}
#'
#' and the keys of the elements will be strings like \code{"key1"},
#' \code{"key2"}, and \code{"key3"}, respectively.
#'
#' If the \code{keyname} is missing, the \code{value} will not be added.
#'
#' As an alternative, you can use the latter two arguments to add element.
#'
#' \code{key=keyname, val=value}
#'
#' Note that any element with the key \code{""} (empty string) will not be added.
#' }
#'
#' \item{\code{delete(key)}}{
#' The method \code{delete} removes the element with the key \code{key} in the dictionary.
#'
#' Suppose that the key name of the element that you want to remove is "keyname".
#' Both of the following two sentences are valid:
#'
#' \code{instance$delete("keyname")}
#'
#' \code{instance$delete(keyname)}
#' }
#'
#' }
#'
#' @keywords RDict
#'
#' @examples
#'
#' ### create a new instance
#'
#' # to create a new instance of the class
#' dict <- RDict$new()
#'
#' # of course you can start to add elements when creating the instance
#' # the previous RDict instance will be removed by doing so
#' # and the memory allocated for that one will be cleared,
#' # as now dict has been pointed to another instance of the class.
#' dict <- RDict$new(id0001=1, id0002=2, collapse=list(id0003=3, id0004=4))
#' # the following sentence is equivalent to the above
#' dict <- RDict$new(id0001=1, id0002=2, id0003=3, id0004=4)
#' # where the three lists are inserted into the dictionary
#'
#' ### immutable methods
#'
#' dict$keys
#' dict$values
#'
#' dict$has(id0001)
#' dict$has("id0005")
#' # TRUE as it has the key attribute
#'
#' dict$get(id0006)
#' dict$get("id0002")
#'
#' ### mutable methods
#'
#' dict$add(id0005=5)
#'
#' dict$add(key="id0006", val=6)
#' # TRUE
#'
#' dict$delete(id0001)
#'
#' @export
RDict <- R6Class("RDict", inherit = RDLL, portable = FALSE, class = FALSE)

RDict$set("public", "initialize", function(..., collapse=NULL){
  items = c(list(...), as.list(collapse))
  keys = names(items)

  for(iter in seq_along(items)){
    if(keys[iter]=="") next
    add(key=keys[iter], val=items[[iter]])
  }
})

RDict$set("public", "has", function(key){
  key = gsub('"',"", deparse(substitute(key)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))

  current <- .head
  while(!is.null(current)){
    if(current$Val$key == key) return(TRUE)
    current <- current$Next
  }
  return(FALSE)
})

RDict$set("public", "get", function(key){
  key = gsub('"',"", deparse(substitute(key)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))

  current <- .head
  while(!is.null(current)){
    if(current$Val$key == key) return(current$Val$val)
    current <- current$Next
  }
  return(NULL)
})

RDict$set("public", "delete", function(key){
  key = gsub('"',"", deparse(substitute(key)))
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

RDict$set("public", "add", function(..., key="", val=NULL){
  items = list(...); keys = names(items)

  for(iter in seq_along(items)){
    if(keys[iter] == "") next
    if(!has(keys[iter])) super$append(list(key=keys[iter], val=items[[iter]]))
  }

  tmp = try(is.character(key), silent=T)
  if(class(tmp) == "try-error") key = deparse(substitute(key))
  if(key != "") if(!has(key)) super$append(list(key=key, val=val))
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
