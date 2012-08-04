preEventSummary <- function(eventCount,data,stepsBefore){
	res <- .Call("preEventSummary",invector=eventCount,invectorTwo=data,hoursBefore=stepsBefore,PACKAGE="hydroEFS")
	return(res)
}