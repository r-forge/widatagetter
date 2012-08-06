getSiteInfo<-function(site_number){
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
		"filter_values":{"station":"SITENUMBER"}}
}
</c-gensym5>
<c-gensym7 xsi:type="xsd:int">
1000000
</c-gensym7>
</JSonCall>
</soap:Body>
</soap:Envelope>\n
'
dataRequestCode <- gsub('SITENUMBER',site_number,dataRequestCode) # Number of requested site.
# create an object to save the results.
h<-basicTextGatherer()
# Request the data.
curlPerform(url="http://203.3.195.115/cgi/webservice.server.pl",
            httpheader=c(Accept="text/xml", Accept="multipart/*", SOAPAction='"http://203.3.195.115/Hydstra#JSonCall"',
                         'Content-Type' = "text/xml; charset=utf-8"),
            postfields=dataRequestCode,
            writefunction = h$update,
            verbose = FALSE
            )
            
            
returnedString<-h$value()

breakOne <- strsplit(returnedString,'<s-gensym3 xsi:type=\\"xsd:string\\">')
breakTwo <- strsplit(breakOne[[1]][2],'<\\/s-gensym3>')[[1]][1]
breakTwo <- gsub('\\/',' ',breakTwo)
parser=newJSONParser()
parser$addData(breakTwo)
x<-parser$getObject()$return$rows[[1]]

results<-list(siteName = x$stname,siteShortName = x$shortname,siteNumber=x$station,siteType=x$stntype,latitude = as.numeric(x$latitude),longitude = as.numeric(x$longitude),elevation = as.numeric(x$elev),geoDatum = x$lldatum,zone=x$zone,comments = x$comment,stillActive = x$active)

return(results)
}
