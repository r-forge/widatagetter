beforeAfterMax <- function(streamProperty, stepsTillMaxmimum, events){
	res<-.Call("bamaximum",invector=streamProperty,eventmax= stepsTillMaxmimum,inevents=events,PACKAGE="hydroEFS")
	return(res)
}