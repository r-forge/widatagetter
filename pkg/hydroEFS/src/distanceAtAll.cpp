#include "distanceAtAll.h"

using namespace Rcpp;

SEXP distanceatallpoints(SEXP invector){
	try {
		NumericVector stream(invector);
		NumericVector out(stream.size());
		
		LogicalVector naCheck = is_na(stream);
		
		int i = 1;
		int counter = 0;
		int eventCount = 0;
		//Because i = 1 we must set out(0)=0, using the counter;
		out(0)=counter;
		
		while(i<stream.size()){
			if(naCheck(i)){
				out(i)=NA_REAL;
				counter++;
			}else{
				if(naCheck(i-1)&&stream(i)>0&&eventCount<stream(i)){
					counter=0;
					out(i)=counter;
					counter++;
				}else{
					if(naCheck(i-1)){
						out(i)=counter;
						counter++;
					} else {
						if(stream(i)==0){
							out(i)=counter;
							counter++;
						} else {
							if(stream(i)>0&&stream(i-1)>0&&stream(i)==stream(i-1)){
								out(i)=counter;
								counter++;
							} else {
								if(stream(i)>0&&stream(i-1)>0&&stream(i-1)!=stream(i)){
									counter=0;
									out(i)=counter;
									counter++;
									eventCount=stream(i);
								} else {
									if(stream(i)>0&&stream(i-1)==0){
										counter=0;
										out(i)=counter;
										counter++;
										eventCount=stream(i);
									}
								}
							}
						}
					}
				}
			}					
			
			i++;
		}
		return List::create(Named("distanceFromLastEvent")=out);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
}
