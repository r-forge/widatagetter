beforeAfterMax <- function(eventCount, event_steps_till_max, event_length){
	res<-.Call("bamaximum",invector=eventCount,eventmax= event_steps_till_max,inevents=event_length,PACKAGE="hydroEFS")
	return(res)
}