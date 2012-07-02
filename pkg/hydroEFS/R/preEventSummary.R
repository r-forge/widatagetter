preEventSummary <- function(events,data,hoursBefore){
	res <- .Call("preEventSummary",invector=events,invectorTwo=data,hoursBefore=hoursBefore,PACKAGE="hydroEFS")
	return(res)
}