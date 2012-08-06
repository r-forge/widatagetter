getSitesWithinCircle<- function(latitude,longitude,degrees,mustBeActive=T){
	
dataRequestCode <- '<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"

xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"

soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"

xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body>
<JSonCall xmlns="http://203.3.195.115/Hydstra">
<c-gensym5 xsi:type="xsd:string">
{ "function":"get_db_info", 
	"version": 1, 
	"params": {
		"return_type": "array", 
		"table_name": "SITE", 
		"geo_filter" : {
			"circle":["X","Y","DEGREES"]
			}
			}
			}

</c-gensym5>
<c-gensym7 xsi:type="xsd:int">
1000000
</c-gensym7>
</JSonCall>
</soap:Body>
</soap:Envelope>\n
'

dataRequestCode <- gsub('X', latitude, dataRequestCode)
dataRequestCode <- gsub('Y', longitude,dataRequestCode)
dataRequestCode <- gsub('DEGREES',degrees,dataRequestCode)


# create an object to save the returned results from the server
h<-basicTextGatherer()
#cat('Sending request to the server\n') 
# send the request to the server.
curlPerform(url="http://203.3.195.115/cgi/webservice.server.pl",
            httpheader=c(Accept="text/xml", Accept="multipart/*", SOAPAction='"http://203.3.195.115/Hydstra#JSonCall"',
                         'Content-Type' = "text/xml; charset=utf-8"),
            postfields=dataRequestCode,
            writefunction = h$update,
            verbose = FALSE
           )
# get the results and put them into an object as a character array.
returnedString<-h$value()
# Check if everyting worked out ok
more<-strsplit(returnedString,'xsi:type=\"xsd:string\">')[[1]][2]
errStart<-regexpr('\\\"error_msg\\\":\\\"',more)
errStart<-regexpr('\\\"error_msg\\\":\\\"',more)
if(errStart!=-1){
temp<-substr(more,as.numeric(errStart)+attributes(errStart)$match.length,nchar(more))
errEnd<-regexpr('\\\",\\\"',temp)
error<-substr(temp,1,errEnd-1)
stop(paste('Server responded: ', error))
}
cat("Got the results, now cleaning them up. This can take some time. Please be PATIENT")
# Now to straighten out the results.
breakOne <- strsplit(returnedString,'<s-gensym3 xsi:type=\\"xsd:string\\">')
breakTwo <- strsplit(breakOne[[1]][2],'<\\/s-gensym3>')[[1]][1]
breakTwo <- gsub('\\/',' ',breakTwo)
parser=newJSONParser()
parser$addData(breakTwo)
tempList<-parser$getObject()
#Simplify the list
tempList2<-tempList$return$rows
#Check wether to not include no longer active sites.
if(mustBeActive){
index <- do.call('c',lapply(tempList2,function(x) x$active==TRUE))
} else{
index <- rep(TRUE,length(tempList))
}
#Creae a new list based on the above info.
newList<-tempList2[index]
##Crappy way to do i, but use a for loop to get the 
results<-lapply(newList,function(x) list(siteName = x$stname,siteShortName = x$shortname,siteNumber=x$station,siteType=x$stntype,latitude = as.numeric(x$latitude),longitude = as.numeric(x$longitude),elevation = as.numeric(x$elev),geoDatum = x$lldatum,zone=x$zone,comments = x$comment,stillActive = x$active))


return(results)
}
