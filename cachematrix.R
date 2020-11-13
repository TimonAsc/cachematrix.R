## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #create a function named makeCacheMatrix with a matrix (square) as argument
        inv <- NULL #create an empty object 
        set <- function(y) { #create a function with the input also being expected to be a matrix 
                x <<- y #give x the value of the y (inputargument) in a different environment than the current environment
                inv <<- NULL #create a similar object inv in the other environment and clear any previous valeu of inv by prior execution of cacheSolve
        } #close set function
        get <- function() x #function to get x from different environment
        setinv <- function(solve) inv <<- solve #function to set the inverse matrix of inv in the other environment
        getinv <- function() inv #function to get the inverse of inv 
        list(set = set, get = get, setinv  = setinv, getinv = getinv)   #create new special matrix by running it into a list and return it to the other environment
                                                                        ## in this way, we can use the $ to retreive the functions by name
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #start the function which return the inverse matrix of input argument ('x')
        inv <- x$getinv() #place the getinv function on the input object
        if(!is.null(inv)) {     #check if the inverse from the other environment is zero. If this value is not zero, we have an inverse matrix that was
                                #was already cached and therefore the cached data will be returned. 
                message("getting cached data") #create a message to inform the user that the cached inverse is used
                return(inv) #return the inverse from the other environment
        }
        data <- x$get() #get matrix of input object 
        inv <- solve(data, ...) #calculate the inverse matrix for the input argument
        x$setinv(inv) #set the inverse in the input object using the created set function
        inv #print the inverse matrix into the environment
}
