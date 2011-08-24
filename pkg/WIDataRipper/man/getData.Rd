\name{getData}
\alias{getData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getData(site_number, start_time, end_time, variable_number = 100, interval = "hour", multiplier = 1, data_type = "mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{site_number}{
%%     ~~Describe \code{site_number} here~~
}
  \item{start_time}{
%%     ~~Describe \code{start_time} here~~
}
  \item{end_time}{
%%     ~~Describe \code{end_time} here~~
}
  \item{variable_number}{
%%     ~~Describe \code{variable_number} here~~
}
  \item{interval}{
%%     ~~Describe \code{interval} here~~
}
  \item{multiplier}{
%%     ~~Describe \code{multiplier} here~~
}
  \item{data_type}{
%%     ~~Describe \code{data_type} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(site_number,start_time,end_time,variable_number=100,interval='hour',multiplier=1,data_type='mean'){

### Here is the SOAP code that is sent to the server for the request of data.
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
"start_time": "STARTTIME",
"varfrom": "VARIABLENUMBER",
"interval": "INTERVAL",
"multiplier": "MULTI",
"varto": "VARIABLENUMBER",
"datasource": "A",
"end_time": "ENDTIME",
"data_type": "DATATYPE",
"multiplier": "1"},
"function": "get_ts_traces",
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
# the next block of text changes the request code with the parameters entered into the function.
dataRequestCode <- gsub('SITENUMBER',site_number,dataRequestCode) # Number of requested site.
dataRequestCode <- gsub('STARTTIME',start_time,dataRequestCode) # the starting time of the record
dataRequestCode <- gsub('VARIABLENUMBER',variable_number,dataRequestCode) # the database number of the desired variable
dataRequestCode <- gsub('INTERVAL',interval,dataRequestCode) # the units of time interval
dataRequestCode <- gsub('MULTI',multiplier,dataRequestCode) # the amount of units per interval.
dataRequestCode <- gsub('ENDTIME',end_time,dataRequestCode) # the ending time of the record
dataRequestCode <- gsub('DATATYPE',data_type,dataRequestCode) # aggregating function.


# create an object to save the returned results from the server
h<-basicTextGatherer()
cat('Sending request to the server\n')
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
cat('Server responded, now just cleaning up the response.\n')
more<-strsplit(returnedString,'xsi:type=\"xsd:string\">')[[1]][2]
###Need to change error checking. if there is an error, there generally is an error message within traces. 
#[1] "{\"error_num\":0,\"buff_required\":520,\"return\":{\"traces\":[{\"error_num\":126,\"error_msg\":\"No data within specified period\",\

# With no error below is -1
errStart<-regexpr('\\\"error_msg\\\":\\\"',more)
if(errStart!=-1){
temp<-substr(more,as.numeric(errStart)+attributes(errStart)$match.length,nchar(more))
errEnd<-regexpr('\\\",\\\"',temp)
error<-substr(temp,1,errEnd-1)
stop(paste('Server responded: ', error))
  }
# Need to add quality code definition.
qualityStart<-regexpr('\\\"quality_codes\\\":\\{\\\"',more)
qualityEnd<-regexpr('\\\"\\},\\\"trace',more)
qualityCodes<-substr(more,qualityStart+attributes(qualityStart)$match.length,qualityEnd)
qualityCodes<-gsub('\\\":\\\"',',',qualityCodes)
qualityCodes<-gsub('\\\",\\\"','\n',qualityCodes)
qualityCodes<-gsub('\"','\n',qualityCodes)
cat(qualityCodes,file='temp.data')
qualityCodes<-read.csv('temp.data',header=F)
unlink('temp.data')

shortStart<-regexpr('short_name\\\":\\"',more)
shortEnd<-regexpr('\\\",\\\"name\\\":\\\"',more)
shortName<-substr(more,as.numeric(shortStart)+attributes(shortStart)$match.length,shortEnd-1)
siteEnd<-regexpr('\"},\"quality_codes\"',more)
siteName<-substr(more,shortEnd+attributes(shortEnd)$match.length,siteEnd-1)
# Now for the data
dataStart<-regexpr('\"trace\":\\[\\{',more)
dataEnd<-regexpr(',\"varfrom_details\"',more)
data<-substr(more,dataStart+attributes(dataStart)$match.length,dataEnd)
data<-gsub('}\\],','\n',data)
data<-gsub('\\},\\{','\n',data)
data<-gsub('\"v\":\"','',data)
data<-gsub('\",\"t\":',',',data)
data<-gsub('\"q\":','',data)
cat(data,file='temp.data')
temp<-read.csv('temp.data',header=F)
unlink('temp.data')
# format the time strings
temp[,2]<-formatC(temp[,2],format='f',digits=0)
data<-data.frame(date=strptime(temp[,2],'\%Y\%m\%d\%H\%M\%S'),x=temp[,1],qualityCode=temp[,3])

# Get more details about the dataset.
xShortStart<-regexpr('varfrom_details\":\\{\"short_name\":\"',more)
additionalData<-substr(more,xShortStart+attributes(xShortStart)$match.length,nchar(more))
xShortEnd<-regexpr('\",\"subdesc\"',additionalData)
xShortName<-substr(additionalData,1,xShortEnd-1)

xUnitStart<-regexpr('\"units\":',additionalData)
xUnitEnd<-regexpr(',\"name\":\"',additionalData)

xUnit<-substr(additionalData,xUnitStart+attributes(xUnitStart)$match.length+1,xUnitEnd-2)

xLongEnd<-regexpr('\"\\},\"site\":\"',additionalData)
xLongName<-substr(additionalData,xUnitEnd+attributes(xUnitEnd)$match.length,xLongEnd-1)

siteNumberEnd<-regexpr('\",\"varto_details',additionalData)
siteNumber<-substr(additionalData,xLongEnd+attributes(xLongEnd)$match.length,siteNumberEnd-1)

# fix the x variable. 
names(data)[2]<-paste(gsub(' ','_',xLongName))
allTogether<-list(data=data,siteNumeber=as.numeric(siteNumber),units=xUnit,siteShortName=shortName,siteName=siteName,qualityCodes=qualityCodes,siteName=siteName)
cat('Make sure you check the quality codes. 255 = missing data, but data is represented by 0\'s.\n')
return(allTogether)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
