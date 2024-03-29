/*
 *  eventFinding.cpp
 *  
 *
 *  Created by Jason Lessels on 6/06/11.
 *  Copyleft - so enjoy
 *
 */

#include "eventFinding.h"

using namespace Rcpp;

SEXP eventType1(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP heightIn, SEXP lengthOfEventIn ){
	//Create a vector of the original inputted values
	try {
		NumericVector streamHeight(streamIn);
        int streamHeight_n = streamHeight.size();
		//Create a vector ready for input
		NumericVector eventCount(streamHeight_n);
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight_n);
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
	//	double rate;
	//	double height;
		steps = as<int>(stepsIn);
        //		rate = as<double>(rateIn);
        //		height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		//Rate and height are now vectors to allow for different samplig patterns.
		NumericVector rate(rateIn);
		NumericVector height(heightIn);
		
		int i = steps;
		int event = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight_n) {
			while(naCheck[i]){//Needs to be a while loop to skip the large na sets.
				eventCount[i]=NA_REAL;
				i=i++;
			}
            
			if (streamHeight[i]>height[i]) {
				for (int j = (i+1); j <= (i+steps); j++) {
					if (!naCheck[j]){
						if ( (streamHeight[j]-streamHeight[i]) > rate[i]) {
							event = 1;
						}
					}
				}
			}
            
			if (event==1) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight_n){
					
					if(naCheck[startOfEvent]){
						eventCount[startOfEvent]=NA_REAL;
						streamDirection[startOfEvent]=NA_REAL;
					} else {
						
						eventCount[startOfEvent]=eventNumber;
						
						if(naCheck[startOfEvent-1]){
							streamDirection[startOfEvent]=NA_REAL;
						} else {
							if(streamHeight[startOfEvent]>streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=1;
							if(streamHeight[startOfEvent]<streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=2;
							if((streamHeight[startOfEvent]-streamHeight[startOfEvent-1])==0) streamDirection[startOfEvent]=3;
						}
					}
					
					startOfEvent++;
					
				}
				eventNumber++;
				i=startOfEvent;
			}
			i++;
			event=0;
		}
		
		
		return List::create(Named("eventDirection")=streamDirection,Named("eventCount")=eventCount);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}





///And now for event type 2

SEXP eventType2(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP heightIn, SEXP lengthOfEventIn ){
	//Create a vector of the original inputted values
	try {
		NumericVector streamHeight(streamIn);
        int streamHeight_n = streamHeight.size();
		//Create a vector ready for input
		NumericVector eventCount(streamHeight_n);
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight_n);
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
        //		double rate;
        //		double height;
		steps = as<int>(stepsIn);
        //		rate = as<double>(rateIn);
        //		height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		//Rate and height are now vectors to allow for different samplig patterns.
		NumericVector rate(rateIn);
		NumericVector height(heightIn);
		
		
		int i = steps;
		int event = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight_n) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			//The below loop is being used from version 0.04.
			for (int j = (i+1); j <= (i+steps); j++) {
				if (!naCheck[j]){
					if ( (streamHeight[j]-streamHeight[i]) > rate[i] && streamHeight[i] > height[i] ) {
						event = 1;
					}
				}
			}
            
			
			if (event==1) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight_n&&streamHeight[startOfEvent]>height[i]){
					
					if(naCheck[startOfEvent]){
						eventCount[startOfEvent]=NA_REAL;
						streamDirection[startOfEvent]=NA_REAL;
					} else {
						
						eventCount[startOfEvent]=eventNumber;
						
						if(naCheck[startOfEvent-1]){
							streamDirection[startOfEvent]=NA_REAL;
						} else {
							if(streamHeight[startOfEvent]>streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=1;
							if(streamHeight[startOfEvent]<streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=2;
							if((streamHeight[startOfEvent]-streamHeight[startOfEvent-1])==0) streamDirection[startOfEvent]=3;
						}
					}
					
					startOfEvent++;
					
				}
				eventNumber++;
				i=startOfEvent;
			}
			i++;
			event=0;
		}
		return List::create(Named("eventDirection")=streamDirection,Named("eventCount")=eventCount);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}


SEXP eventType3(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP lengthOfEventIn ){
	try {
		//Create a vector of the original inputted values
		NumericVector streamHeight(streamIn);
        int streamHeight_n = streamHeight.size();
		//Create a vector ready for input
		NumericVector eventCount(streamHeight_n);
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight_n);
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
	//	double rate;
		
		steps = as<int>(stepsIn);
		//rate = as<double>(rateIn);
		length = as<int>(lengthOfEventIn);
		//Rate and height are now vectors to allow for different samplig patterns.
		NumericVector rate(rateIn);
	//	NumericVector height(heightIn);
		
		
		int i = steps;
		int event = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight_n) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			//The below loop is being used from version 0.04.
			for (int j = (i+1); j <= (i+steps); j++) {
				if (!naCheck[j]){
					if ( (streamHeight[j]-streamHeight[i]) > rate[i] ) {
						event = 1;
					}
				}
			}
            
			if (event==1) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight_n){
					
					if(naCheck[startOfEvent]){
						eventCount[startOfEvent]=NA_REAL;
						streamDirection[startOfEvent]=NA_REAL;
					} else {
						
						eventCount[startOfEvent]=eventNumber;
						
						if(naCheck[startOfEvent-1]){
							streamDirection[startOfEvent]=NA_REAL;
						} else {
							if(streamHeight[startOfEvent]>streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=1;
							if(streamHeight[startOfEvent]<streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=2;
							if((streamHeight[startOfEvent]-streamHeight[startOfEvent-1])==0) streamDirection[startOfEvent]=3;
						}
					}
					
					startOfEvent++;
					
				}
				eventNumber++;
				i=startOfEvent;
			}
			i++;
			event=0;
		}
		return List::create(Named("eventDirection")=streamDirection,Named("eventCount")=eventCount);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}




SEXP eventType4(SEXP streamIn, SEXP heightIn, SEXP lengthOfEventIn ){
	
	try {
		//Create a vector of the original inputted values
		NumericVector streamHeight(streamIn);
        int streamHeight_n = streamHeight.size();
		//Create a vector ready for input
		NumericVector eventCount(streamHeight_n);
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight_n);
		LogicalVector naCheck = is_na(streamHeight);
		
		int length;
        //		double height;
		//height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		//Rate and height are now vectors to allow for different samplig patterns.
        //	NumericVector rate(rateIn);
		NumericVector height(heightIn);
		
		
		int i = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight_n) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			if (streamHeight[i]>height[i]) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight_n){
					
					if(naCheck[startOfEvent]){
						eventCount[startOfEvent]=NA_REAL;
						streamDirection[startOfEvent]=NA_REAL;
					} else {
						
						eventCount[startOfEvent]=eventNumber;
						
						if(naCheck[startOfEvent-1]){
							streamDirection[startOfEvent]=NA_REAL;
						} else {
							if(streamHeight[startOfEvent]>streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=1;
							if(streamHeight[startOfEvent]<streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=2;
							if((streamHeight[startOfEvent]-streamHeight[startOfEvent-1])==0) streamDirection[startOfEvent]=3;
						}
					}
					
					startOfEvent++;
					
				}
				eventNumber++;
				i=startOfEvent;
			}
			i++;
		}
		return List::create(Named("eventDirection")=streamDirection,Named("eventCount")=eventCount);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}


///Event type 5 
SEXP eventType5(SEXP streamIn, SEXP heightIn, SEXP lengthOfEventIn ){
	
	try {
		//Create a vector of the original inputted values
		NumericVector streamHeight(streamIn);
        int streamHeight_n = streamHeight.size();
		//Create a vector ready for input
		NumericVector eventCount(streamHeight_n);
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight_n);
		LogicalVector naCheck = is_na(streamHeight);
		
		int length;
//		double height;
		//height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		//Rate and height are now vectors to allow for different samplig patterns.
	//	NumericVector rate(rateIn);
		NumericVector height(heightIn);
		
		
		int i = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight_n) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			if (streamHeight[i]>height[i]) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight_n&&streamHeight[startOfEvent]>height[i]){
					
					if(naCheck[startOfEvent]){
						eventCount[startOfEvent]=NA_REAL;
						streamDirection[startOfEvent]=NA_REAL;
					} else {
						
						eventCount[startOfEvent]=eventNumber;
						
						if(naCheck[startOfEvent-1]){
							streamDirection[startOfEvent]=NA_REAL;
						} else {
							if(streamHeight[startOfEvent]>streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=1;
							if(streamHeight[startOfEvent]<streamHeight[startOfEvent-1]) streamDirection[startOfEvent]=2;
							if((streamHeight[startOfEvent]-streamHeight[startOfEvent-1])==0) streamDirection[startOfEvent]=3;
						}
					}
					
					startOfEvent++;
					
				}
				eventNumber++;
				i=startOfEvent;
			}
			i++;
		}
		return List::create(Named("eventDirection")=streamDirection,Named("eventCount")=eventCount);
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}