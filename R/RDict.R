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
#' The keys of the elements in the dictionary are stored as strings.
#' The values in the dictionary are not necessarily to be of the same type,
#' and they can be any R objects.
#'
#' @author Yukai Yang, \email{yukai.yang@@statistik.uu.se}
#'
#' @section References:
#' For the details about the dictionary data structure, see \href{https://en.wikipedia.org/wiki/Associative_array}{Dictionary at Wikipedia}.
#'
#' @seealso \link{R6DS} for the introduction of the reference class and some common methods
#'
#' @section Immutable Methods:
#'
#' The immutable methods do not change the instance.
#'
#' \describe{
#'
#' \item{\code{has(key)}}{
#' The method \code{has} returns a boolean indicating
#' if the dictionary contains the element with the key "\code{key}".
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
#' The method \code{values} returns a list of the values in the dictionary (unnamed list).
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
#' \item{\code{add(key, val)}}{
#' The method \code{add} adds a new element (the pair key and val) into the dictionary.
#' It will not add element with the key which exists already in the dictionary.
#' It returns a boolean showing if the adding is successful.
#'
#' Note that any element with the key \code{""} (empty string) will not be added.
#' }
#'
#' \item{\code{add_multiple(..., collapse=NULL)}}{
#' The method \code{add_multiple} adds new elements into the dictionary.
#' It will not add element with the key which exists already in the dictionary.
#'
#' The argument \code{...} stands for any input with the form
#'
#' \code{keyname1 = value2, keyname2 = value2, ...}
#'
#' Therefor, the input can take the form
#'
#' \code{instance$add(key1=1, key2="hello", key3=list(1))}
#'
#' and the keys of the elements will be strings like \code{"key1"},
#' \code{"key2"}, and \code{"key3"}, respectively.
#'
#' If the \code{keyname} is missing, the \code{value} will not be added.
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
#'
#' It returns a boolean showing if the element is found and deleted.
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
#' dict$add(id0005, 5)
#'
#' dict$add(key="id0006", val=6)
#'
#' dict$delete(id0001)
#'
#' @export
RDict <- R6Class("RDict", portable = FALSE, class = FALSE)

RDict$set("private", ".elem", list())

RDict$set("active", "size", function(){ return(length(.elem)) })

RDict$set("public", "initialize", function(..., collapse=NULL){
  add_multiple(..., collapse=collapse)
})

RDict$set("active", "toList", function(){
  return(.elem)
})

RDict$set("public", "is_empty", function(){
  return(length(.elem) == 0)
})

RDict$set("public", "add", function(key, val){
  key = gsub('"',"", deparse(substitute(key)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))
  if(key != "") if(is.null(.elem[[key]])){ .elem[[key]] <<- val; return(TRUE) }
  return(FALSE)
})

RDict$set("public", "add_multiple", function(..., collapse=NULL){
  items = c(list(...), as.list(collapse))
  keys = names(items)

  if(!is.null(keys)){
    for(iter in seq_along(items)){
      if(keys[iter] == "") next
      if(is.null(.elem[[ keys[iter] ]])) .elem[[ keys[iter] ]] <<- items[[iter]]
    }
  }
})

RDict$set("public", "has", function(key){
  key = gsub('"',"", deparse(substitute(key)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))
  return(!is.null(.elem[[key]]))
})

RDict$set("public", "get", function(key){
  key = gsub('"',"", deparse(substitute(key)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))
  return(.elem[[key]])
})

RDict$set("public", "delete", function(key){
  key = gsub('"',"", deparse(substitute(key)))
  #tmp = try(is.character(key), silent=T)
  #if(class(tmp) == "try-error") key = deparse(substitute(key))
  if(is.null(.elem[[key]])) return(FALSE)
  .elem[[key]] <<- NULL
  return(TRUE)
})

RDict$set("active", "keys", function(){
  return(names(.elem))
})

RDict$set("active", "values", function(){
  return(unname(.elem))
})
