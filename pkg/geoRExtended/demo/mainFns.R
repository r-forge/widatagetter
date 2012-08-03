## A quick demo of the different ways to use the geoRExtended to use likfit.
cat("Using the original geoR method without a nugget\n")
system.time(reml.1 <- likfit(s100, ini=c(0.5, 0.5), fix.nug = TRUE, lik.met = "REML"))
cat("Using the Rcpp version of geoR method without a nugget\n")
system.time(reml.2 <- likfitRcpp(s100, ini=c(0.5, 0.5), fix.nug = TRUE, lik.met = "REML"))
cat("Using the original geoR method with a nugget\n")
system.time(reml.3 <- likfit(s100, ini=c(0.5, 0.5), fix.nug = FALSE, lik.met = "REML"))
cat("Using the Rcpp version of geoR method with a nugget\n")
system.time(reml.4 <- likfitRcpp(s100, ini=c(0.5, 0.5), fix.nug = FALSE, lik.met = "REML"))
cat("Using the Rcpp version of geoR optimising using the GenSA package.\n")
system.time(reml.5 <- likfitSANN(s100, ini=c(0.5, 0.5), fix.nug = FALSE, lik.met = "REML", SAcontrol=list(maxit=1000), limits=pars.limits(phi=c(0,1),sigmasq=c(0,0.5), nugget=c(0,0.1))))
cat("plot the results")
plot(variog(s100))
lines(reml.1)
lines(reml.2)
lines(reml.3)
lines(reml.4)
lines(reml.5)
cat("Perform cross validation using gstat and geoR methods.\n")
krige_data <- data.frame(data=s100$data, x=s100$coords[,1], y=s100$coords[,2])
model. <- vgm(psill=reml.1$sigmasq, "Exp", range=reml.1$phi, reml.1$nugget)
system.time(gstatXV <- krige.cv(formula=data~1, locations=~x+y, data=krige_data,model=model.))
system.time(geoRXV <- xvalid(s100, model=reml.1))
cat("Getting a quick summary of the cross validation results.\n")
LOOCV(geoRXV)
LOOCV(gstatXV)

cat("----------------------------------------\n")
cat("----------------------------------------\n")
cat(" And now to try out the backward elimination method")
data(meuse)
temp <- na.omit(meuse)
geoData <- as.geodata(temp,coords.col=1:2,data.col=5, covar.col = c(3:4,6:13))
formula. <- "~elev+soil+lime+om+soil+dist+landuse"

## Run the full model...
fullModel <- likfit_step(geodata=geoData, formula=formula.,ini.=c(0.3,100),nugget=0.01,lambda=0,fix.lambda=FALSE,fix.nugget=FALSE,lik.method="RML",noBackSelection=TRUE,useRcpp=TRUE)
## And now backwards selection time;
bestModel <- likfit_step(geodata=geoData, formula=formula.,ini.=c(0.3,100),nugget=0.01,lambda=0,fix.lambda=FALSE,fix.nugget=FALSE,lik.method="RML",noBackSelection=FALSE,useRcpp=TRUE)

## Cross validation using gstat
# Transform the values before cross validation - as this is for the theta values. 
temp$zinc <- BCtransform(temp$zinc, bestModel[[3]]$lambda)$data
cvFormula <- paste("zinc", bestModel[[1]],sep="")
gstatCrossValidationBestModel <-krige.cv(as.formula(cvFormula),~x+y,data=temp,model = vgm(bestModel[[3]]$sigmasq,"Exp",bestModel[[3]]$phi,bestModel[[3]]$nugget))
### Using gstat should give the same results, but it will take a little longer
tempModel <- bestModel[[3]]
# Change the models lambda value so it does not back transform the values.
tempModel$parameters.summary["lambda",2] <- 1
# And now for the validation.
geoRCrossValidationBestModel <- xvalid(coords=temp[,1:2],data=temp$zinc, model=tempModel)
# Get a summary of the cross valiation (Your hoping thetaMean=1, and thetaMedian=0.45)
LOOCV(geoRCrossValidationBestModel)
# Get a summary of the cross valiation (Your hoping thetaMean=1, and thetaMedian=0.45)
LOOCV(gstatCrossValidationBestModel)

