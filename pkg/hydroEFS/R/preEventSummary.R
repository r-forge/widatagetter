preEventSummary <- function(events,data,stepsBefore){
	res <- .Call("preEventSummary",invector=events,invectorTwo=data,hoursBefore=stepsBefore,PACKAGE="hydroEFS")
	return(res)
}