#include "beforeAfterMaximum.h"

using namespace Rcpp;

SEXP bamaximum(SEXP invector, SEXP eventmax, SEXP inevents){
	try{
		NumericVector invec(invector);
		NumericVector maxPos(eventmax);
		NumericVector eventLength(inevents);
		LogicalVector naCheck = is_na(invec);
		int invec_n = invec.size();
		NumericVector outvec(invec_n);
		
		int i = 0;
		int eventNumber = 0;
		int startOfEvent = 0;
		
		while(i<invec_n){
			//  std::cout <<i << std::endl;
			if(naCheck(i)){
				outvec(i) = NA_REAL;
			} else {
				if(invec(i)==0){
					outvec(i) = 0;
				} else {
					if(invec(i)>0){
						eventNumber = invec(i);
						startOfEvent = i;
						// std::cout << eventNumber <<std::endl;
						//  std::cout << i << std::endl;
						while(i<(startOfEvent+eventLength(eventNumber-1))){
							if(naCheck(i)){
								outvec(i) = NA_REAL;
							} else {
								if(i<=(startOfEvent+maxPos(eventNumber-1))){
									outvec(i) = 1;
								} else {
									outvec(i) = 2;
								}
							}
							i++;
						}
					}
					
				}
			}
			i++;
		}
		return List::create(Named("beforeAfterMaximum") = outvec);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
}

