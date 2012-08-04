###Michaels better neater version of tprobs
likfit_prob <- function(x.reml,x.lm){
  names(x.reml$beta) <- names(x.lm$coefficients)
 if(length(x.reml$beta.var)==1){
	 t.val <- x.reml$beta/sqrt(x.reml$beta.var)
         d.f <- length(x.lm$model[, 1]) - x.reml$npar
    t.prob <- pt(abs(t.val), d.f, lower.tail = FALSE)
    res<-data.frame(estimate = x.reml$beta, standard.error = sqrt(x.reml$beta.var), 
        t_value = t.val, t_prob = t.prob)

	 } else {
    t.val <- x.reml$beta/sqrt(diag(x.reml$beta.var))	 
        d.f <- length(x.lm$model[, 1]) - x.reml$npar
    t.prob <- pt(abs(t.val), d.f, lower.tail = FALSE)
    res<-data.frame(estimate = x.reml$beta, standard.error = sqrt(diag(x.reml$beta.var)), 
        t_value = t.val, t_prob = t.prob)
    	}
return(res)
}
### Summary of either gstat or geoR cross validation result.
LOOCV <- function (Xvalid) {
	ret <- list()
	# This function now handles the results from either gstat or geoR.
	if(class(Xvalid)=="data.frame"){
		ret$RMSE <- sqrt(sum((Xvalid$observed-Xvalid$var1.pred)^2)/length(Xvalid$observed))
		ret$ME <- sum((Xvalid$observed-Xvalid$var1.pred))/length(Xvalid$observed)
		ret$thetaMean <- mean(((Xvalid$observed-Xvalid$var1.pred)^2)/Xvalid$var1.var)
		ret$thetaMedian <- median(((Xvalid$observed-Xvalid$var1.pred)^2)/Xvalid$var1.var)
	} else {
		ret$RMSE <- sqrt(sum((Xvalid$data-Xvalid$predicted)^2)/length(Xvalid$data))
		ret$ME <- sum((Xvalid$data-Xvalid$predicted))/length(Xvalid$data)
		ret$thetaMean <- mean(((Xvalid$data-Xvalid$predicted)^2)/Xvalid$krige.var)
		ret$thetaMedian <- median(((Xvalid$data-Xvalid$predicted)^2)/Xvalid$krige.var)
	}
	return(ret)
}