arma::mat arma_dist(arma::mat coords){
    int size = coords.n_rows;
    arma::mat dists(size,size);
    for(int i =0;i<size;i++){
        for(int j = 0;j<size;j++){
            dists(j,i) = sqrt(pow((coords(i,0)-coords(j,0)),2)+pow((coords(i,1)-coords(j,1)),2));
        }
    } 
    return dists;
}

//This is a simple c version of the matern function. It still relies on the R gamma function and the R bessel function.
double matern(double u, double phi, double kappa){
    double uphi = u/phi;
    if ( uphi > 0 ) {
        uphi = (pow(2,(-(kappa-1)))/Rf_gammafn(kappa))*pow(uphi,kappa)*Rf_bessel_k(uphi,kappa,1);
    } else {
        uphi = 1;
    }
    if ( uphi > ( 600 * phi ) ) {
        uphi = 0;
    }
    return uphi;
}
//This is an arma version of the cov.spatial function. It does not handle simulations, it requires covModel to be declared and it returns a square matrix not a triganular matrix.
arma::mat covSpatial(arma::mat coords, std::string covModel, arma::vec kappa, double phi, double sigmasq){
    arma::mat dist = arma_dist(coords);
    
    arma::mat covs(dist.n_rows,dist.n_cols);
    
    if(phi< 1e-16){
        covModel = "pure.nugget";
    }
    //Tidy up kappa.
    if ( accu (kappa) == 0 ) {
        kappa(0) = 0.5;
    }
    
    arma::mat objSc = dist/phi;
    int covRows = covs.n_rows;
    int covCols = covs.n_cols;
    
    //Now its time to go through all the different models. Let the fun begin.
    if(covModel=="pure.nugget"){
        covs.zeros();
    }
    if(covModel=="wave"){
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                covs(j,i) = ( 1 / dist(j,i) ) * ( phi * sin ( objSc(j,i) ) );
            }
        }
    }
    if(covModel=="exponential"){
        covs = exp( - (objSc));
    }
    if(covModel=="matern"){ 
        if ( kappa(0) == 0.5) {
            for ( int i =0; i < covCols; i++ ) {
                for ( int j = 0; j < covRows; j++ ) {
                    covs(j,i) = exp ( - objSc(j,i) );
                }
            }
        } else {
            for ( int i =0; i < covCols; i++ ) {
                for ( int j = 0; j < covRows; j++ ) {
                    covs(j,i) = matern( dist(j,i), phi, kappa(0));
                }
            }
        }
    }
    if(covModel=="gausian"){
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                covs(j,i) = exp( - (pow( objSc(j,i), 2 ) ) );
            }
        }
    }
    if ( covModel == "spherical" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                if ( dist(j,i) < phi ){
                    covs(i,j) = (1 - 1.5 * objSc(j,i)) + 0.5 * pow(objSc(j,i),3);
                } else {
                    covs(i,j) = 0;
                }
            }
        }
    }
    
    if ( covModel == "circular" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                if ( objSc(j,i) > 1 ) {
                    objSc(j,i) = 1;
                }
                if ( dist(j,i) < phi){
                    covs(j,i) = ( 1 - ( 2 * ( (objSc(j,i) * sqrt ( 1 - pow(objSc(j,i),2)) + asin(objSc(j,i)))))/arma::math::pi());
                } else {
                    covs(j,i) = 0;
                }
            }
        }
    }
    
    if ( covModel == "cubic" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                if(dist(j,i) < phi){
                    covs(j,i) = ( 1 - ( 7 * pow(objSc(j,i),2) - 8.75 *  pow(objSc(j,i),3) + 3.5*  pow(objSc(j,i),5) - 0.75 *  pow(objSc(j,i),7)));
                } else {
                    covs(j,i) = 0;
                }
            }
        }
    }
    
    if ( covModel == "power" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                covs(j,i) = pow( dist(j,i), phi);
            }
        }
    }
    if ( covModel == "powered.exponential" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                covs(j,i) = exp( - (pow(objSc(j,i),kappa(0) ) ) );
            }
        }
    }
    if ( covModel == "cauchy" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                covs(j,i) = pow( 1 +  pow ( objSc(j,i) , 2 ) , -kappa(0) );
            }
        }
    }
    if ( covModel == "gneiting" ) {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                objSc(j,i) = 0.301187465825 * objSc(j,i);
                if( ( 1 - objSc(j,i)) > 0){
                    covs(j,i) = ( 1 + 8 * objSc(j,i) + 25 * pow ( objSc(j,i) , 2 ) + 32 * pow ( objSc(j,i) , 3 ) ) * pow(1-objSc(j,i),8);
                } else {
                    covs(j,i) = 0;
                }
            }
        }
    }
    if ( covModel == "gencauchy") {
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                covs(j,i) = pow ( 1+ pow (objSc(j,i) ,kappa(1) ) , - ( kappa(0) / kappa(1) ) );
            }
        }
    }
    if ( covModel == "gneiting.matern" ) {
        objSc = 0.301187465825 * objSc/kappa(1);
        for ( int i =0; i < covCols; i++ ) {
            for ( int j = 0; j < covRows; j++ ) {
                if( ( 1 - objSc(j,i) ) > 0){
                    covs(j,i) = ( 1 + 8 * objSc(j,i) + 25 * pow(objSc(j,i),2) + 32 * pow(objSc(j,i),3)) * pow( ( 1 - objSc(j,i) ), 8) * matern( dist(j,i), phi, kappa(0));
                } else {
                    covs(j,i) = 0;
                }
            }
        }
    }
    if ( covModel == "power" ) {
        double A;
        arma::vec gammaRes(4);
        gammaRes.zeros();
        int index = 0;
        arma::vec gammaVals(2);
        gammaVals(0)=1;
        gammaVals(1)=2;
        
        for(int i = 0;i<2;i++){
            for(int j = 0;j<2;j++){
                gammaRes(index) = ( Rf_gammafn ( phi + ( 1.0 + gammaVals(i) )  / 2.0 ) /  ( Rf_gammafn ( 1+ phi )* Rf_gammafn( ( 1.0 +  gammaVals(j) ) / 2.0 ) ) ) ;
                index++;
                
            }
        }
        
        double b = gammaRes.max();
        
        double covMax = covs.max();
        A = covMax / sqrt ( arma::math::pi() ) * Rf_gammafn ( (1+phi ) / 2 ) * Rf_gammafn ( 1 - ( phi / 2 ) );
        A = A * b;
        covs = A - covs;
        covMax = covs.max();
        covs = covs/covMax;
        
        
        
    }
    for ( int i =0; i < covCols; i++ ) {
        for ( int j = 0; j < covRows; j++ ) {
            if ( dist(j,i) < 1e-16 ) {
                covs(j,i) = sigmasq;
            } else {
                covs(j,i) = sigmasq * covs(j,i);
            }
        }
    }
    //Check for a bad value in the cov.spatial matrix.
    if(!covs.is_finite())Rwarn("Either a Inf,Na or NaN value in cov.spatial");
    
    return covs;
}