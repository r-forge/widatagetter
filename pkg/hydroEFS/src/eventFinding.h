#ifndef _hydroEFS_EVENTFINDING_H
#define _hydroEFS_EVENTFINDING_H

#include <RcppArmadillo.h>

RcppExport SEXP eventType1(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP heightIn, SEXP lengthOfEventIn ) ;
RcppExport SEXP eventType2(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP heightIn, SEXP lengthOfEventIn ) ;
RcppExport SEXP eventType3(SEXP streamIn, SEXP stepsIn, SEXP rateIn, SEXP lengthOfEventIn ) ;
RcppExport SEXP eventType4(SEXP streamIn, SEXP heightIn, SEXP lengthOfEventIn ) ;
RcppExport SEXP eventType5(SEXP streamIn, SEXP heightIn, SEXP lengthOfEventIn ) ;
#endif