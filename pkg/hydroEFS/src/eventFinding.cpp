/*
 *  eventFinding.cpp
 *  
 *
 *  Created by Jason Lessels on 6/06/11.
 *  Copyright 2011 __MyCompanyName__. All rights reserved.
 *
 */

#include "eventFinding.h"

using namespace Rcpp;

SEXP eventType1(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP heightIn, SEXP lengthOfEventIn ){
	//Create a vector of the original inputted values
	try {
		NumericVector streamHeight(streamIn);
		//Create a vector ready for input
		NumericVector eventCount(streamHeight.size());
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight.size());
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
		double rate;
		double height;
		steps = as<int>(stepsIn);
		rate = as<double>(rateIn);
		height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		
		
		int i = steps;
		int event = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight.size()) {
			while(naCheck[i]){//Needs to be a while loop to skip the large na sets.
				eventCount[i]=NA_REAL;
				i=i++;
			}
			//The below loop is being used from version 0.04.
			if (streamHeight[i]>height) {
				for (int j = (i+1); j <= (i+steps); j++) {
					if (!naCheck[j]){
						if ( (streamHeight[j]-streamHeight[i]) > rate) {
							event = 1;
						}
					}
				}
			}
/* --- This has to be changed as it goes backwards, where the SCA method goes forwards.
 //This was taken out of action after 0.03.
			for(int j = (i-1);j>(i-steps-1);j--){ 
				if(!naCheck[j]){
					if((streamHeight[i]-streamHeight[j])>rate&&streamHeight[i]>height){
						event=1;
						
					}
				}
			}

 */
			if (event==1) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight.size()){
					
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
		//Create a vector ready for input
		NumericVector eventCount(streamHeight.size());
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight.size());
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
		double rate;
		double height;
		steps = as<int>(stepsIn);
		rate = as<double>(rateIn);
		height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		
		
		int i = steps;
		int event = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight.size()) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			//The below loop is being used from version 0.04.
			for (int j = (i+1); j <= (i+steps); j++) {
				if (!naCheck[j]){
					if ( (streamHeight[j]-streamHeight[i]) > rate && streamHeight[i] > height ) {
						event = 1;
					}
				}
			}
			/*
			for(int j = (i-1);j>(i-steps-1);j--){ 
				if(!naCheck[j]){
					if((streamHeight[i]-streamHeight[j])>rate&&streamHeight[i]>height){
						event=1;
						
					}
				}
			}
			 */
			
			if (event==1) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight.size()&&streamHeight[startOfEvent]>height){
					
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
		//Create a vector ready for input
		NumericVector eventCount(streamHeight.size());
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight.size());
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
		double rate;
		
		steps = as<int>(stepsIn);
		rate = as<double>(rateIn);
		length = as<int>(lengthOfEventIn);
		
		
		
		int i = steps;
		int event = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight.size()) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			//The below loop is being used from version 0.04.
			for (int j = (i+1); j <= (i+steps); j++) {
				if (!naCheck[j]){
					if ( (streamHeight[j]-streamHeight[i]) > rate ) {
						event = 1;
					}
				}
			}
			/*
			for(int j = (i-1);j>(i-steps-1);j--){ 
				if(!naCheck[j]){
					if((streamHeight[i]-streamHeight[j])>rate){
						event=1;
						
					}
				}
			}
			*/
			if (event==1) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight.size()){
					
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
		//Create a vector ready for input
		NumericVector eventCount(streamHeight.size());
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight.size());
		LogicalVector naCheck = is_na(streamHeight);
		
		int steps;
		int length;
		double height;
		height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		
		
		int i = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight.size()) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			

			if (streamHeight[i] > height) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight.size()){
					
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
		//Create a vector ready for input
		NumericVector eventCount(streamHeight.size());
		//CReate a vector ready for input
		NumericVector streamDirection(streamHeight.size());
		LogicalVector naCheck = is_na(streamHeight);
		
		int length;
		double height;
		height = as<double>(heightIn);
		length = as<int>(lengthOfEventIn);
		
		
		
		int i = 0;
		int startOfEvent = 0;
		int endOfEvent = 0;
		int eventNumber = 1;
		int counter=0;
		
		while(i<=streamHeight.size()) {
			while(naCheck[i]){
				eventCount[i]=NA_REAL;
				i=i++;
			}
			
			if (streamHeight[i]>height) {
				
				startOfEvent=i;
				endOfEvent=i+length;
				
				
				while (startOfEvent<endOfEvent&&startOfEvent<streamHeight.size()&&streamHeight[startOfEvent]>height){
					
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