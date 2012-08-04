distanceBetweenEvents <- function(eventCount){
	res <- .Call("distance",invector=eventCount,PACKAGE="hydroEFS")
	return(res)
}