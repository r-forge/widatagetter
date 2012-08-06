likfit_step <- function(geodata,formula,ini.,...,noBackSelection=FALSE, useRcpp=TRUE){
	totalCovariates <- length(str_match_all(formula,"\\+")[[1]])+1
	for(i in 1:totalCovariates){
		##Create the trend of the model
		trend.d <- trend.spatial(as.formula(formula),geodata)
		##run the model.
		if(useRcpp){
			model <- likfitRcpp(geodata,ini.cov.pars=ini.,trend=trend.d,...)
		} else {
			model <- likfit(geodata,ini.cov.pars=ini.,trend=trend.d,...)
		}
		## create a data set for the lm model
		dataSet <- data.frame(data=BCtransform(geodata$data,lambda=model$lambda),geodata$covar)
		## get the probablities of each covariate
		tProbs <-likfit_prob(model,lm(as.formula(paste("data",formula,sep="")),data=dataSet))
		if(noBackSelection){
			break
		}
		## Find which variable to drop out.
		dropOutIndex <- tProbs[,"t_prob"]>=0.05& tProbs[,"t_prob"]==max(tProbs[,"t_prob"])
		## If there are no variables to drop out then exit the loop. The final model has been selected.
		if(sum(dropOutIndex)==0){
			cat("Backward selection: final model",formula,"\n")
			break
        }
		#Split the string into individual covariates
		covariates <- strsplit(formula,"[\\+\\~]")[[1]][-1]
		# Is the variable a factor:level. if it is have to be delicate
		if(!rownames(tProbs)[dropOutIndex] %in% covariates){
			for(i in 1:length(covariates)){
				temp <- grepl(covariates[i],rownames(tProbs[dropOutIndex,]))
				if(temp){
					dropOut <- i
				}
			}
			## All of the levels must be non significant.
			counter <- vector(length= length(tProbs[,1]))
			significance <- vector(length= length(tProbs[,1]))
			for(i in 1:length(tProbs[,1])){
				temp<-grepl(covariates[dropOut], rownames(tProbs[i,]))
				if(temp){
					counter[i] <- TRUE
					if(tProbs[i,"t_prob"]>=0.05){
						significance[i] <- TRUE
					} else {
						significance[i] <- FALSE
					}
				} else {
					counter[i] <- FALSE
				}
			}
			## Now determine if the factor should be dropped
			if(sum(significance)==sum(counter)){
				cat("Backward selection: Dropping out",covariates[dropOut],"\n")
				covariates <- covariates[-dropOut]
				# If the factor shouldn't be dropped out then find which one should be.
			} else {
                dropOut <- match(rownames(tProbs)[dropOutIndex],covariates)
                cat("Backward selection: Dropping out",covariates[dropOut],"\n")
                covariates <- covariates[-dropOut]
			}
		} else {
			dropOut <- match(rownames(tProbs)[dropOutIndex],covariates)
			cat("Backward selection: Dropping out",covariates[dropOut],"\n")
			covariates <- covariates[-dropOut]
		}
		
		
        # Put the formula back together
		formula<-paste("~",paste(covariates,collapse="+"),sep="")
		cat("Backward selection: The updated formula is", formula,"\n")
    }
    # return the final model, the probablities and the formula.
	return(list(formula=formula,probs=tProbs,likfit_model=model))
}