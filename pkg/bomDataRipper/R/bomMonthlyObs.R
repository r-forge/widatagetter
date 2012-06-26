bomMonthlyObs <-
function(siteNumber,observation="temp") {
  
  if(class(observation)!="character") stop("observation must be; temp, rain or solar.")
  observation <- tolower(observation)
  if(is.na(match(observation,c("rain","temp","solar")))) stop("observation must be; temp, rain or solar.")
  
  #139 = rain, 36 = temp, 203 = solar
  # Some BOM sites start with a 0 (e.g 068166). If the R siteNumber is in numeric format, the 0 will be lost, so the for loop checks this and acts accordingly.
  if(is.numeric(siteNumber)){
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=0",siteNumber,sep="")
  } else {
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=",siteNumber,sep="")
  }
  if(observation=="rain") {
    theurl <- gsub("DATATYPE", "139",theurl)
    dataCode <- 139
  }
  if(observation=="temp") {
    theurl <- gsub("DATATYPE", "36",theurl)
    dataCode <- 36
  }
  if(observation=="solar") {
    theurl <- gsub("DATATYPE", "203",theurl)
    dataCode <- 203
  }
  # make the orginal request to the server - this is needed to get a unique number from the server.
  # Gets the data from the webpage
  tables <- readHTMLTable(theurl)
  if(length(tables)==0){stop("Unfortunately there are no data available for the site number you have entered.
This may be because either the station number is invalid, or the station has
                             not observed the type of data requested.")
  }
  #removes unwanted rows. On the website the data is presented in groups of 25, so there are some odd rows here and there
  removeIndex <-tables[[1]][,1]=="Graph"|tables[[1]][,1]=="top"|tables[[1]][,1]=="Year"
  tables[[1]] <- tables[[1]][!removeIndex,]
  
  monthlyObs <- as.numeric(as.matrix(t(tables[[1]][, -c(1,14)])))
  year <- rep(as.numeric(as.matrix(tables[[1]][, c(1)])),each=12)
  month <- rep(1:12, length=length(year))
  tables[[1]] <- data.frame(year, month, observation=monthlyObs)
  # Change the names of the two tables returned. The first table has the monthly data, and the second table has summary stats
  names(tables) <- c("monthly","summaryStats")
  return(tables)
}
