#include "distance.h"

using namespace Rcpp;

SEXP distance(SEXP invector){
	try{
		Rcpp::NumericVector in(invector);
		arma::vec armaIn(in);
		int maX = armaIn.max();
		Rcpp::NumericVector out(maX);
		int i = 0;
		int counter = 0;
		int eventCounter = 1;
		while(i<armaIn.n_rows){
			if(armaIn(i)!=eventCounter){
				counter++;
			} else {
				out(eventCounter-1)=counter;
				eventCounter++;
				counter=0;
			}
			i++;
			
		}
		
		return Rcpp::DataFrame::create(Named("distance") = out);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}
