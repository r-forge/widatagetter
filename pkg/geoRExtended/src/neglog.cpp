#include "neglog.h"
using namespace Rcpp;

SEXP neglog(SEXP par, SEXP fpIn, SEXP ipIn, SEXP tempIn){
    try{
        //Convert the R objects to c++ objects.
        Rcpp::List fp(fpIn);
        Rcpp::List ip(ipIn);
        Rcpp::List temp_list(tempIn);
        int p = Rcpp::as<int>(temp_list["beta.size"]);
        arma::mat coords = Rcpp::as<arma::mat>(temp_list["coords"]);
        Rcpp::NumericVector pars(par);
        double phi = pars[0];
        
        //Set the model parameters to boolean
        bool ipFKappa = Rcpp::as<bool>(ip["f.kappa"]);
        bool ipFTausq = Rcpp::as<bool>(ip["f.tausq"]);
        bool ipFLambda = Rcpp::as<bool>(ip["f.lambda"]);
        bool ipFPsiR = Rcpp::as<bool>(ip["f.psiR"]);
        bool ipFPsiA = Rcpp::as<bool>(ip["f.psiA"]);
        
        double log_jacobian;
        if ( ipFLambda ) {
            log_jacobian = Rcpp::as<double>(temp_list["log.jacobian"]);
        } else {
            log_jacobian = 0;
        }
        
        
        
        double sigmasq;
        double tausq;
        double kappa;
        double lambda;
        double psiR;
        double psiA;
        
        if ( ipFTausq ) {
            double fpTausq = Rcpp::as<double>(fp["tausq"]);
            if ( fpTausq > 0 ){
                int npars_min = pars.size();
                sigmasq = pars[npars_min-1];
            } else {
                sigmasq = 1.0;
            }
        } else {
            sigmasq = 1.0;
        }
        
        int count = 1; //Starts at 1 as phi is pars(0) - declared above.
        if (! ipFTausq ){
            tausq = pars[count];
            count++;
        } else {
            tausq = Rcpp::as<double>(fp["tausq"]);;
        }
        if (! ipFKappa ){
            kappa = pars[count];
            count++;
        } else {
            kappa = Rcpp::as<double>(fp["kappa"]);
        }
        if (! ipFLambda ){
            lambda = pars[count];
            count++;
        } else {
            lambda = Rcpp::as<double>(fp["lambda"]);
        }
        if (! ipFPsiR){
            psiR = pars[count];
            count++;
        } else {
            psiR = Rcpp::as<double>(fp["psiR"]);
        }
        if (! ipFPsiA){
            psiA = pars[count];
        } else {
            psiA = Rcpp::as<double>(fp["psiA"]);
        }	
        bool print_pars = Rcpp::as<bool>(temp_list["print.pars"]);
        
        if ( print_pars ){
            //		if ( ipFTausq ) {
            //			double fpTausq = Rcpp::as<double>(fp["tausq"]);
            //			if( fpTausq > 0 ){
            Rprintf("\nphi \t tausq \t kappa \t psiA \t psiR \t lambda");
            Rprintf("\n %f", phi);
            Rprintf(" \t %f",tausq);
            Rprintf(" \t %f",kappa);
            Rprintf(" \t %f",psiA);
            Rprintf(" \t %f",psiR);
            Rprintf(" \t %f",lambda);
            //			}
            //		}
        }
        NumericVector allParams = NumericVector::create(phi,tausq,sigmasq,kappa);
        if ( kappa < 1e-04 || (tausq + sigmasq) < 1.490116e-08 || is_true(any(allParams <0))){
            
            return(wrap(1.340781e+154));
        }
        
        Rcpp::NumericVector  data = Rcpp::as<Rcpp::NumericVector>(temp_list["z"]);
        arma::vec arma_data(data.begin(),data.size(),true);
        if ( !ipFLambda ){
            if(std::abs(lambda-1) < 1e-04 ){
                log_jacobian = 0;
            } else {
                if ( is_true ( any ( data <= 0 ) ) ) {
                    Rf_error("Transformation not allowed for zero or negative data");
                }
                arma_data = pow(data,lambda-1);
                arma::uvec negativeData = arma_data<=0;
                double uvecSum = sum(negativeData);
                if ( uvecSum>0) {
                    log_jacobian = log(prod(arma_data));
                } else {
                    log_jacobian = accu(log(arma_data));
                }
            }
            if (  std::abs(lambda)  < 1e-04 ){
                arma_data = log(data);
            } else {
                arma_data = (pow ( data, lambda ) - 1)/lambda;
            }
        } else {
            arma_data = data;
        }
        
        int n = Rcpp::as<int>(temp_list["n"]);
        Rcpp::List Xmat = temp_list["xmat"];
        arma::mat xmat = Rcpp::as<arma::mat>(Xmat[0]);
        std::string covModel = Rcpp::as<std::string>(temp_list["cov.model"]);
        std::string methodLik = Rcpp::as<std::string>(temp_list["method.lik"]);
        double logLikCTE = Rcpp::as<double>(temp_list["loglik.cte"]);
        
        
        //For the results
        double negloglik =0;
        double sumnegloglik = 0;
        
        
        double vlogDet = 0;
        arma::mat varcov(coords.n_rows,coords.n_rows);
        varcov.zeros();
        arma::vec tausqSig = varcov.diag();
        tausqSig.fill(tausq + sigmasq);
        if(phi < 1e-16 || sigmasq < 1e-16){
            if(ipFTausq){
                varcov.diag() = tausqSig;
                vlogDet = (n/2.0)*log(tausq + sigmasq);
            } else {
                varcov.diag().fill(tausq+1);
                vlogDet = (n/2.0)*log(1 + tausq);
            }
        } else {
            arma::vec armaKappa(2);
            armaKappa(0) = kappa;
            armaKappa(1) = 0;
            if(sigmasq < 1e-10 || phi < 1e-10){
                varcov.diag().fill( tausq + sigmasq );
            } else {
                varcov = covSpatial(coords,covModel,armaKappa,phi,sigmasq);
                varcov.diag().fill( tausq + sigmasq );
            }
            arma::mat varcovSqrt;
            
            if(chol(varcovSqrt,varcov)){
                
                vlogDet = trace(log(varcovSqrt));
                
            } else {
                arma::mat U;
                arma::vec s;
                arma::mat V;
                if(svd(U,s,V,varcov)){
                    vlogDet = accu(trunc_log(sqrt(s)));
                    varcovSqrt = trans(trans(U)*sqrt(sqrt(s)))*(trans(U)*sqrt(sqrt(s)));
                } else {
                    arma::vec eigval;
                    arma::mat eigvec;
                    eig_sym(eigval,eigvec,varcov);
                    vlogDet = accu(log(sqrt(eigval)));
                    varcovSqrt = trans(trans(eigvec) * sqrt(sqrt(eigval)))*(trans(eigvec) * sqrt(sqrt(eigval)));
                }
            }
            
        }
        
        
        arma::mat ivx = solve(varcov,xmat);
        
        arma::mat xivx = trans(ivx)*xmat;
        arma::vec z = arma_data;
        arma::vec betahatA =solve ( xivx, ( trans ( ivx ) * z ) );
        arma::mat res = z-xmat*betahatA;
        double ssres = as_scalar ( trans ( res ) * solve ( varcov , res ) );
        
        
        
        if ( methodLik == "ML" ) {
            if ( ipFTausq && tausq > 0 ) {
                negloglik = vlogDet + 0.5 * ssres;
            } else {
                negloglik = (n/2.0) * log(ssres) + vlogDet;
            }	
        }
        if ( methodLik == "RML" ) {
            
            double choldet;
            if ( xivx.n_elem == 1 ){
                double Xivx = xivx[0,0];
                choldet = 0.5*log(Xivx);
            } else {
                arma::mat cholXivx = chol(xivx);
                choldet = trace(log(cholXivx));
            }
            if ( ipFTausq && tausq > 0 ) {
                negloglik = vlogDet + 0.5*ssres + choldet;
                
            } else {
                negloglik = ((n-p)/2.0) * log(ssres) + vlogDet + choldet;
                
            }
        }
        
        negloglik = negloglik - logLikCTE;
        sumnegloglik = sumnegloglik +negloglik;
        sumnegloglik = sumnegloglik - log_jacobian;
        if(sumnegloglik > (1.340781e+154) || sumnegloglik == INFINITY ||sumnegloglik == -INFINITY ){
            sumnegloglik = 1.340781e+154;	
        }
        if(print_pars){
            Rprintf("\nlog-likelihood = -%f", sumnegloglik);
        }
        return(wrap(sumnegloglik));
    } catch(std::exception&__ex__){
        forward_exception_to_r(__ex__);
    }catch(...){
        ::Rf_error("c++ exception (unknown error)");
    }
}

//Create a warning function and a print function. To send warnings and print statements to R.
