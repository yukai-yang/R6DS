##########################################################################################
# Node
##########################################################################################

# The node reference class
#
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
