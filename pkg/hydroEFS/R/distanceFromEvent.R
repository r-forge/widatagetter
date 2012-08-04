distanceFromEvent <- function(eventCount){
	res <- .Call("distanceatallpoints",invector=eventCount,PACKAGE="hydroEFS")
	return(res)
}