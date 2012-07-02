#ifndef _hydroEFS_EVENTSUMMARY_H
#define _hydroEFS_EVENTSUMMARY_H

#include <RcppArmadillo.h>

RcppExport SEXP eventSummary(SEXP invector, SEXP invectorTwo) ;
RcppExport SEXP preEventSummary(SEXP invector, SEXP invectorTwo, SEXP hoursBefore);

#endif