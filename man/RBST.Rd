% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RBST.R
\docType{data}
\name{RBST}
\alias{RBST}
\title{The RBST reference class}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
RBST
}
\description{
The RBST reference class implements the data structure binary search tree (BST).
}
\details{
A BST is a particular type of container storing elements in nodes by following a binary tree structure.
So the element is the value of the corresponding node in the tree.

The BST has one root on top, which is the first node of the tree,
and each node in the BST has at most two sub-nodes (left sub-node and right sub-node) which can be the roots of their sub-trees.

The BST should be equipped with the "<" and "=" operations such that any two nodes in the tree can be compared.
Note that, by the definitions of the "<" and "=" operations, the operation ">" is also defined.

The BST structure follows strictly the rules that, for a certain node in the tree,
any nodes in its left sub-tree must be strictly smaller ("<") than it,
any nodes in its right sub-tree must be strictly larger (">") than it,
and any two nodes in the tree must not be equal (no "=").

Therefore, the BST is a special set or dictionary equipped with "<", ">" operations.

When you create a new RBST instance, you have to input two functions which defines
the bodies of the two private methods \code{lessthan} and \code{equal}.
The RBST instance then will use them to make comparison and decide where to put new nodes (build the BST).

Each time a new node is inserted, the BST algorithm finds its location on the tree.
Then you can imagine, the BST is efficient in maintaining (inserting and deleting), searching and traversing the tree.
An average O(log n) time complexity can be achieved by applying the BST algorithm.

A very important fact is that, the RBST only compares the nodes by using
the function \code{equal}.
So it will regard any two nodes identical if \code{equal} returns \code{TRUE},
even though they are different.

We see that the BST can also be regarded as a dictionary,
as the key of the dictionary is actually the value input into \code{insert}, \code{delete} and \code{search_for}.

The traversals of the BST (in-order, pre-order, and post-order) are implemented as well.
A \code{callback} function can be input into the \code{traverse} function
to specify how to treat the traversed nodes.
By default (if you do not input anything here) the \code{traverse} function
prints the traversed nodes.
But of course you can, for example, store them by changing the \code{callback} function,
see the examples below.

The elements in the BST are not necessarily to be of the same type,
and they can even contain functions.
}
\section{References}{

For the details about the BST data structure, see \href{https://en.wikipedia.org/wiki/Binary_search_tree}{BST at Wikipedia}.
}

\section{Class Method}{


The class method belongs to the class.

\describe{

\item{\code{new(lessthan, equal, ..., collapse=NULL)}}{
The \code{new} method creates a new instance of the RBST class
containing the values in \code{...} and \code{collapse} as its nodes.

The argument \code{lessthan} takes a function defining the "<" operation,
and the argument \code{equal} takes a function defining the "=" operation.
Both of the functions takes two values of the nodes in the tree and return
a boolean.

\code{lessthan} can take, for example, the form

\code{lessthan <- function(x, y) return(x$num < y$num)}

where \code{x} and \code{y} are values of two nodes in the tree
with the attribute \code{num}.


\code{equal} can take, for example, the form

\code{equal <- function(x, y) return(x$num == y$num)}

where \code{x} and \code{y} are values of two nodes in the tree
with the attribute \code{num}.
}

}
}

\section{Immutable Methods}{


The immutable methods do not change the nodes of the instance.

\describe{

\item{\code{toList}, \code{toList_pre}, and \code{toList_post}}{
The active method \code{toList} returns a list containing its elements (a copy).

The order of the list can be "traverse-in-order" by using \code{toList},
"traverse-pre-order" by using \code{toList_pre},
or "traverse-post-order" by using \code{toList_post}
}

\item{\code{traverse(mode, callback=function(item){print(item)}, ...)}}{
The \code{traverse} method takes at least two arguments which are \code{mode} and \code{callback}.

The \code{mode} takes a value in one of the three strings
\code{"in"}, \code{"pre"}, and \code{"post"} which indicate
\emph{traverse-in-order}, \emph{traverse-pre-order}, and \emph{traverse-post-order}, respectively.

The \code{callback} takes a function
specifying how to handle the value of each node in the tree.
By default, \code{callback} prints the nodes by using the \code{print} function.

Note that the first argument of the \code{callback} function must be the value of the node
but not the node itself!

\code{callback} can have two or more arguments.
The method also takes \code{...} as the additional arguments for the \code{callback} function if any.
}

\item{\code{search_for(val)}}{
The method \code{search_for} uses the \code{equal} function
to compare \code{val} with the nodes in BST.
It returns the value of the node if the node is \code{equal} to the given value, and \code{NULL} otherwise.

As the tree has been structured strictly by following the rules introduced above,
there is no need to search the whole tree in most cases, and the maintaining and searching are efficient.
}

\item{\code{min}}{
The active method \code{min} returns the smallest node in the tree,
and \code{NULL} if the tree is empty.
}

\item{\code{max}}{
The active method \code{min} returns the largest node in the tree,
and \code{NULL} if the tree is empty.
}

}
}

\section{Mutable Methods}{


The mutable methods changes the nodes of the instance.

\describe{

\item{\code{insert(..., collapse=NULL)}}{
The method \code{insert} inserts new nodes into the tree.
If some nodes are \code{equal} to the nodes in the tree,
they will not be inserted.
}

\item{\code{delete(val)}}{
The method \code{delete} removes the node which is \code{equal} to \code{val}.
If the node is found, then it will be removed and the function returns a \code{TRUE},
and if the node is not found, then it will do nothing and returns a \code{FALSE},
}

}
}

\examples{

### create a new instance

# you have to define two functions for "<" and "="
lessthan <- function(x, y) return(x$key < y$key)
equal <- function(x, y) return(x$key == y$key)
# remember that the nodes in the BST have the "key" variable
# and it is numeric

# to create a new instance of the class
bst <- RBST$new(lessthan=lessthan, equal=equal)

# of course you can start to push elements when creating the instance
bst <- RBST$new(lessthan=lessthan, equal=equal,
    list(key=5, val="5"), collapse=list(list(key=3,val="3"), list(key=9,val="9")))
# the following sentence is equivalent to the above
bst <- RBST$new(lessthan=lessthan, equal=equal,
    list(key=5, val="5"), list(key=3,val="3"), list(key=9,val="9"))
# where the three lists are inserted into the BST

### maintaining

bst$insert(list(key=5, val="6"))
bst$insert(list(key=6, val="5"))

bst$delete(list(key=7, val="7"))
# FALSE
bst$delete(list(key=6, val="7"))
# TRUE and delete list(key=6, val="5")
# though val are different

### searching

bst$search_for(list(key=0, val="0"))
# NULL
bst$search_for(list(key=5, val="0"))
# the BST has a node whose key is 5

### min and max

# min and max are two active functions
# so the parenthesis is not needed
bst$min
bst$max

### toList

bst$toList
bst$toList_pre
bst$toList_post

### traversing

# by default, the callback function prints the nodes
# but you can re-define the callback function
queue <- RQueue$new()
callback <- function(item)queue$enqueue(item)
# remember that RQueue is a reference class
# so the new callback will store the traversed nodes

bst$traverse(mode = "in", callback=callback)
tmp = queue$dequeue(); print(tmp)
while(!is.null(tmp)) {tmp = queue$dequeue(); print(tmp)}
bst$traverse(mode = "in", callback=callback)
tmp = queue$dequeue(); print(tmp)
while(!is.null(tmp)) {tmp = queue$dequeue(); print(tmp)}

# pre-order traversing
bst$traverse(mode = "pre", callback=callback)
tmp = queue$dequeue(); print(tmp)
while(!is.null(tmp)) {tmp = queue$dequeue(); print(tmp)}

# post-order traversing
bst$traverse(mode = "post", callback=callback)
tmp = queue$dequeue(); print(tmp)
while(!is.null(tmp)) {tmp = queue$dequeue(); print(tmp)}

}
\seealso{
\link{R6DS} for the introduction of the reference class and some common methods
}
\author{
Yukai Yang, \email{yukai.yang@statistik.uu.se}
}
\keyword{RBST}
