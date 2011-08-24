\name{getSiteVariables}
\alias{getSiteVariables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSiteVariables(site_number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{site_number}{
%%     ~~Describe \code{site_number} here~~
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
function(site_number){

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
"datasource": "A",
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
res[i,1]<-paste(strptime(do.call('cbind',temp[[1]][i])[4,1],'\%Y\%m\%d\%H\%M\%S'))
res[i,2]<-paste(strptime(do.call('cbind',temp[[1]][i])[2,1],'\%Y\%m\%d\%H\%M\%S'))
res[i,3]<-do.call('cbind',temp[[1]][i])[6,1]
res[i,4]<-do.call('cbind',temp[[1]][i])[8,1]
res[i,5]<-do.call('cbind',temp[[1]][i])[10,1]
res[i,6]<-do.call('cbind',temp[[1]][i])[12,1]
  }


# chuck it all into a list and return to the user.
results<-list(siteName=siteName,siteShortName=shortName,siteNumber=site_number,variables=res)
return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
