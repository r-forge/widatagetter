findEvents <- function(streamProperty,steps,rate,height,lengthOfEvent,eventType){
	###TODO:::Add error checking within R, before the R objects are sent to c++. REMOVE WHEN COMPLETE. 
	if(length(rate)==1){
        rate<- rep(rate, length(streamProperty))
    }
	if(length(height)==1){
        height<- rep(height, length(streamProperty))
    }
    if(eventType<1||eventType>5) stop("eventType must be an integer between 1 and 5")
    
	switch(eventType,
	res<-.Call("eventType1",streamIn=as.numeric(streamProperty),stepsIn=as.integer(steps),rateIn=as.numeric(rate),heightIn=as.numeric(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType2",streamIn=as.numeric(streamProperty),stepsIn=as.integer(steps),rateIn=as.numeric(rate),heightIn=as.numeric(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType3",streamIn=as.numeric(streamProperty),stepsIn=as.integer(steps),rateIn=as.numeric(rate),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType4",streamIn=as.numeric(streamProperty),heightIn=as.numeric(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
	res<-.Call("eventType5",streamIn=as.numeric(streamProperty),heightIn=as.numeric(height),lengthOfEventIn=as.integer(lengthOfEvent),PACKAGE="hydroEFS"),
)
return(res)
}