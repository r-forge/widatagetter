getSiteVariables <-
function(site_number,data_source="A"){

dataRequestCode <- '<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"

xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"

soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"

xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body>
<JSonCall xmlns="http://203.3.195.115/Hydstra">
<c-gensym5 xsi:type="xsd:string">
{"params":
{"site_list": "SITENUMBER",
"datasource": "DATASOURCE",
},
"function": "get_variable_list",
"version": 1}
}
</c-gensym5>
<c-gensym7 xsi:type="xsd:int">
1000000
</c-gensym7>
</JSonCall>
</soap:Body>
</soap:Envelope>\n
'
# set the site number to that from the user.
dataRequestCode <- gsub('SITENUMBER',site_number,dataRequestCode) # Number of requested site.
dataRequestCode <- gsub('DATASOURCE',data_source,dataRequestCode) # Number of requested site.
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
# rip the response from the RCurl object.
returnedString<-h$value()

# split the string, the header is boring.
more<-strsplit(returnedString,'xsi:type=\"xsd:string\">')[[1]][2]
#Get the names of the site.
shortStart<-regexpr('short_name\\\":\\"',more)
shortEnd<-regexpr('\\\",\\\"name\\\":\\\"',more)
shortName<-substr(more,as.numeric(shortStart)+attributes(shortStart)$match.length,shortEnd-1)
siteEnd<-regexpr('\"},\"variables\\\":\\[\\{\\\"',more)
siteName<-substr(more,shortEnd+attributes(shortEnd)$match.length,siteEnd-1)
if(siteName=='')stop('This site doesn\'t seem to exist')

# find the start and the end of the list of varaibles within the response.
variables<-substr(more,siteEnd+attributes(siteEnd)$match.length,nchar(more))
variablesEnd<-regexpr('\\}\\]',variables)
variables<-substr(variables,1,variablesEnd-2)
variablesList<-strsplit(variables,'\\\"\\},\\{\\\"')

# use some nice fast lapply functions to clean and seperate the each variable
temp<-lapply(variablesList,function(x) gsub('\\\":\\\"', ',',x))
temp<-lapply(temp,function(x) gsub('\\\",\\\"', ',',x))
temp<-lapply(temp,function(x) gsub('\\\\','',x))
temp<-lapply(temp,function(x) strsplit(x,','))

# now use a dirty for loop to get all the variables into a nice neat data frame.
res<-data.frame(startingDate=NA,endingDate=NA,subdesc=NA,variable=NA,units=NA,name=NA)
for(i in 1:length(temp[[1]])){
res[i,1]<-paste(strptime(do.call('cbind',temp[[1]][i])[4,1],'%Y%m%d%H%M%S'))
res[i,2]<-paste(strptime(do.call('cbind',temp[[1]][i])[2,1],'%Y%m%d%H%M%S'))
res[i,3]<-do.call('cbind',temp[[1]][i])[6,1]
res[i,4]<-do.call('cbind',temp[[1]][i])[8,1]
res[i,5]<-do.call('cbind',temp[[1]][i])[10,1]
res[i,6]<-do.call('cbind',temp[[1]][i])[12,1]
}


# chuck it all into a list and return to the user.
results<-list(siteName=siteName,siteShortName=shortName,siteNumber=site_number,variables=res)
return(results)
}

