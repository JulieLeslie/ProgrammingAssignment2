# I am really not sure about this assignment as the logic behind these
# two functions and the example of assignment seems incredibly contrived
# especially when there are much easier ways to do this
# Never the less, here is my offering

#The first function creates a list of functions so you can set a matrix,
# get that matrix again and also calculate the inverse of that matrix


makeCacheMatrix <- function(x = matrix()) {
	#set the value of m to null so that it is definitely contains the value you want
	m<-NULL
	
	#set your matrix into the variable
	set<-function(y){
		#through lexical scoping this is available in the function above, but not the environment
		#that is, I can use it in the other functions but I wont see it in the environment
		x<<-y
		m<<-NULL
	}
	
	# returns the value of the inputed matrix
	get<-function() x
	
	# create the inverse of the matrix, but check it is there first (lexical scoping!)
	setmatrix<-function() {
		if (is.null(x)){
			message("you haven't set the matrix yet")
		}
		else {
			m<<- solve(x)
		}
	}	
	
	# returns the value of the inverse matrix
	getmatrix<-function() m
	
	#return a list of all the functions and matricies
	list(set=set, get=get,
	setmatrix=setmatrix,
	getmatrix=getmatrix)
}


## This function checks to see if the inverse matrix has been created
# input is the list that was created in makeCacheMatrix

cacheSolve <- function(x=list(), ...) {
    # get the value of the inverse matrix
	m <- x$getmatrix()
	n <- x$get()
	#if it is not null then return the value of the inverse matrix
	# don't calculate it again
    if(!is.null(m)){
		return(m)
    }	
	#Otherwise get the value of matrix and calculate
	else if (!is.null(n)) {
		n <- solve(n)
		return(n)
	}
	#If that doesn't work then let the user know
	else {
		message("It has all gone badly wrong")
	}
}

