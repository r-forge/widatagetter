getLatest<- function(site_number,variable_number=100){
### First step is to check that the site has the data source PROV
		siteDataSources<-getSiteDataSources(site_number)$dataSources
		checkSiteHasLatest<-any(match(siteDataSources,"PROV"),1)
		if(length(siteDataSources)==0||!checkSiteHasLatest) stop("Site has no latest values or doesn't exist")
### Next step is to check if the disered variable is in the PROV data source.
		siteVariables<-getSiteVariables(site_number,data_source="PROV")
		whichIsTheOneWeWant<-match(as.numeric(siteVariables$variables[,"variable"]),variable_number,nomatch=0)
		doesVariableExist<-any(whichIsTheOneWeWant,1)
		if(!doesVariableExist)stop("The site has no latest values for that varaible")
### Next step is to get the start and end time.
		end_time<-as.POSIXlt(siteVariables$variables[whichIsTheOneWeWant,"endingDate"],tz="GMT")
		start_time<-end_time-(7*60*60*24) # 7 days ago.
		results<-getData(site_number,format(start_time,"%Y%m%d%H%M%S"),format(end_time,"%Y%m%d%H%M%S"),
			variable_number,data_source="PROV")
			# remove missing values (quality = 255)
	goodDataIndex<-results$data$qualityCode!=255
		results$data<-results$data[goodDataIndex,]
			return(results)
			}


