getSiteDataSources <- function(site_number){ 
dataRequestCode <- '<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"

xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"

soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"

xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body>
<JSonCall xmlns="http://203.3.195.115/Hydstra">
<c-gensym5 xsi:type="xsd:string">
{ "function": "get_datasources_by_site", "version": 1, "params": {
"site_list": "SITENUMBER"}
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
temp<-parser$getObject()
return(list(site=temp$return$sites[[1]]$site,dataSources=temp$return$sites[[1]]$datasources))
}

