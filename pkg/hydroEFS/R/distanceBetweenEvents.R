distanceBetweenEvents <- function(events){
	res <- .Call("distance",invector=events,PACKAGE="hydroEFS")
	return(res)
}