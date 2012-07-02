eventSummary <- function(events,data){
		###TODO:::Add error checking within R, before the R objects are sent to c++. REMOVE WHEN COMPLETE.
	res<-.Call("eventSummary",invector=events,invectorTwo=data,PACKAGE="hydroEFS")
	return(res)
}