#include "eventSummary.h"

using namespace Rcpp;
SEXP eventSummary(SEXP invector, SEXP invectorTwo){
	try {
		//Connvert imput to Rcpp vector
		Rcpp::NumericVector in(invector);
		Rcpp::NumericVector inTwo(invectorTwo);
		//Convert Rcpp imput to armadillo (as a pointer)
		arma::vec armaIn(in);
		arma::vec armaInTwo(inTwo);
		
		//Create a vector for the for loop
		arma::vec temp(armaIn.n_rows,1);
		arma::vec tempWithNA(armaIn.n_rows,1);
		
		int maX = armaIn.max();
		//create a vector for the output.
		//arma::mat out(maX,2);
		Rcpp::NumericMatrix out(maX,8);
		//run a loop over the main vector and find where the condition is met. subset the second vector
		//declare help
		arma::uvec help = find(armaIn==0);
		//Make some int for counting the time to the max of the event.
		int maxFinder;
		
		//Make an indice for filling in out
		for(int i =0;i<maX;i++){
			help = find(armaIn==i+1);
			tempWithNA = armaInTwo.elem(help);
			help = find(tempWithNA>=0);
			temp = tempWithNA.elem(help);
			if(temp.n_rows>0){
				out(i,0) = temp.max();
				out(i,1) = temp.min();
				out(i,2) = mean(temp);
				out(i,3) = median(temp);
				out(i,4) = stddev(temp);
				out(i,5) = var(temp);
				out(i,6) = temp.n_rows;
			}else {
				out(i,0) = NA_REAL;
				out(i,1) = NA_REAL;
				out(i,2) = NA_REAL;
				out(i,3) = NA_REAL;
				out(i,4) = NA_REAL;
				out(i,5) = NA_REAL;
				out(i,6) = NA_REAL;
			}
			if(temp.n_rows<3){//This is to make sure there is atleast 3 observations.
				out(i,3) = NA_REAL;
				out(i,4) = NA_REAL;
				out(i,5) = NA_REAL;
			}
			//I need to add a loop here that starts at the start of the event with a counter to find the amount of steps until the maximum. The maximum is found above.
			if(arma::is_finite(out(i,0))){
				while(maxFinder<tempWithNA.n_rows){
					//This was for testing			std::cout << maxFinder << std::endl;
					if(tempWithNA(maxFinder)==out(i,0))break;
					maxFinder++;
				}
				out(i,7) = maxFinder;
				maxFinder=0;
			} else {
				out(i,7) = NA_REAL;
				maxFinder = 0;//Just to make sure, Im sure this isnt needed.	
			}
			
			maxFinder=0;
			//Check the length of the event here.
		}
		
		return Rcpp::DataFrame::create(Rcpp::Named("maximum") = out.column(0),
									   Rcpp::Named("minimum") = out.column(1),
									   Rcpp::Named("mean") = out.column(2),
									   Rcpp::Named("median") = out.column(3),
									   Rcpp::Named("std_dev") = out.column(4),
									   Rcpp::Named("variance") = out.column(5),
									   Rcpp::Named("Length") = out.column(6),
									   Rcpp::Named("Steps_till_max") = out.column(7));
		
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
	
}



SEXP preEventSummary(SEXP invector, SEXP invectorTwo, SEXP hoursBefore) {
	try {
		int hoursBack = as<int>(hoursBefore);
		
		//Connvert imput to Rcpp vector
		Rcpp::NumericVector in(invector);
		Rcpp::NumericVector inTwo(invectorTwo);
		//Convert Rcpp imput to armadillo (as a pointer)
		arma::vec armaIn(in);//this is the event index vector
		arma::vec armaInTwo(inTwo); // This is the rainfall or discharge- thats not important
		
		int maX = armaIn.max();//Get the amount of events in the eventindex
		//CReate somewhere to store the stats.
		Rcpp::NumericMatrix out(maX,6);
		
		//Now loop throught the event index.
		int i = hoursBack+1;//The extra 1 is due to the start of the while loop
		int eventCount = 0;
		int eventIndex = 0;
		//CReate a vector for the previous 24 hours of observations and a temp one,
		arma::vec tempVec(hoursBack);
		arma::vec subVec(hoursBack);
		//Create a vector to to decide if the values are missing.
		arma::uvec notNA(hoursBack);
		while(i<armaIn.n_rows){
			if(armaIn(i)>0){
				eventCount = armaIn(i); //Set the event count to the current event
				tempVec = armaInTwo.rows((i-1-hoursBack),(i-1));
				notNA = find(tempVec>=0); //Get which values to keep.
				subVec = tempVec.elem(notNA);
				
				if(subVec.n_rows>0){
					out((eventCount-1),0) = subVec.max();
					out((eventCount-1),1) = subVec.min();
					out((eventCount-1),2) = mean(subVec);
					out((eventCount-1),3) = median(subVec);
					out((eventCount-1),4) = stddev(subVec);
					out((eventCount-1),5) = var(subVec);
				} else {
					out((eventCount-1),0) = NA_REAL;
					out((eventCount-1),1) = NA_REAL;
					out((eventCount-1),2) = NA_REAL;
					out((eventCount-1),3) = NA_REAL;
					out((eventCount-1),4) = NA_REAL;
					out((eventCount-1),5) = NA_REAL;		
				}
				if(subVec.n_rows<3){//This is to make sure there is atleast 3 observations.
					out((eventCount-1),3) = NA_REAL;
					out((eventCount-1),4) = NA_REAL;
					out((eventCount-1),5) = NA_REAL;
				}
				while(armaIn(i)==armaIn(i+1)){//skip the rest of the event...
					i++;
				}
			}
			i++;
			
		}
		
		return Rcpp::DataFrame::create(Rcpp::Named("maximum") = out.column(0),
									   Rcpp::Named("minimum") = out.column(1),
									   Rcpp::Named("mean") = out.column(2),
									   Rcpp::Named("median") = out.column(3),
									   Rcpp::Named("std_dev") = out.column(4),
									   Rcpp::Named("variance") = out.column(5));
		
	} catch(std::exception&__ex__){
		forward_exception_to_r(__ex__);
	}catch(...){
		::Rf_error("c++ exception (unknown error)");
	}
}
