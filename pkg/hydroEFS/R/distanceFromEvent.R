distanceFromEvent <- function(events){
	res <- .Call("distanceatallpoints",invector=events,PACKAGE="hydroEFS")
	return(res)
}