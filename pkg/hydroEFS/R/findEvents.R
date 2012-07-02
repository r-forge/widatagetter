findEvents <- function(streamProperty,steps,rate,height,lengthOfEvent,eventType){
	###TODO:::Add error checking within R, before the R objects are sent to c++. REMOVE WHEN COMPLETE. 
	
	switch(eventType,
	res<-.Call("eventType1",streamIn=as.numeric(streamProperty),stepsIn=as.integer(steps),rateIn=as.double(rate),heightIn=as.double(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType2",streamIn=as.numeric(streamProperty),stepsIn=as.integer(steps),rateIn=as.double(rate),heightIn=as.double(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType3",streamIn=as.numeric(streamProperty),stepsIn=as.integer(steps),rateIn=as.double(rate),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType4",streamIn=as.numeric(streamProperty),heightIn=as.double(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType5",streamIn=as.numeric(streamProperty),heightIn=as.double(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
)
return(res)
}