likfit_wald <- function(object, probs=NULL, terms=NULL,levels=NULL,factor=NULL){
	if(!inherits(object,"likGRF")){
		stop("object must be of class likGRF.")
	}
	if(is.null(levels)&&is.null(factor)&&is.null(terms)){
		return(wald.test(b=object$beta,Sigma=object$beta.var, Terms=1:length(object$beta)))
	}
	if(!is.null(factor)&&!is.null(probs)){
		index <- (1:length(object$beta))[grepl(factor,rownames(probs))]
		if(sum(index)==0){
			stop("no matching factor found. You can always specifiy the covariates using the terms argument.")
		}
		return(wald.test(b=object$beta,Sigma=object$beta.var, Terms=as.numeric(index)))
	}
	if(!is.null(terms)){
		return(wald.test(b=object$beta,Sigma=object$beta.var, Terms=terms))
	}
	if(!is.null(levels)){
		if(sum(levels==0)&&length(levels)==length(object$beta)){
			return(wald.test(b=object$beta,Sigma=object$beta.var, L=levels))
		}else{
			stop("levels is either the wrong length or does not sum to 0.")
		}

	}
}