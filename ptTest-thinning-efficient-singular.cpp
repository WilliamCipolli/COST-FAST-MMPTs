// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace arma;
using namespace Rcpp;
using namespace std; 
double pi = 2*acos(0.0);
static double const log2pi = std::log(2.0 * pi);

//mu and sigma for standard Gaussian Distribution
arma::rowvec mu0ARM;
arma::mat sigma0ARM;
NumericVector mu0;
NumericMatrix sigma0;

////////////////////////////////////////////////////////////////
///// Determinant Modulus
///////////////////////////////////////////////////////////////
double getDeterminantMod(NumericMatrix A){
  Environment myEnv = Environment::global_env();
  Function f = myEnv["getDeterminantMod"];
  NumericVector output (1);
  output = f(_["A"]=A);
  return output[0];
}

///////////////////////////////////////////////////////////////
///// Covariance Matrix
///////////////////////////////////////////////////////////////
NumericMatrix cov(NumericMatrix A){
  Rcpp::Environment base("package:stats"); 
  Rcpp::Function f = base["cov"];    
  NumericMatrix output = f(_["x"] = A);
  return output;
}


///////////////////////////////////////////////////////////////
///// random multivariate normal observations!!
///////////////////////////////////////////////////////////////
// [[Rcpp::export]]
arma::mat matTimesVec(arma::mat mat, arma::vec v) {
  for(int i; i < mat.n_cols; i++){
    mat.col(i)  %=  v;
  }
  return mat;
}

NumericVector mvrnorm( NumericVector muv, NumericMatrix sigmav) {
  int nsamp = 1;
  arma::vec muvARM = as<arma::vec> (muv);
  arma::mat sigmavARM = as<arma::mat> (sigmav);
  int ncols = sigmavARM.n_cols;
  arma::mat Y = arma::randn(nsamp, ncols);
  // rewritten below to avoid singularity issue with chol()
  //return as<NumericVector>(wrap(arma::repmat(muvARM, 1, nsamp).t() + Y * arma::chol(sigmavARM)));
  mat U;
  vec d;
  mat V;
  svd(U,d,V,sigmavARM);
  mat svdmat = (V * (matTimesVec(U.t(), sqrt(d)))).t();
  
  return as<NumericVector>(wrap(arma::repmat(muvARM, 1, nsamp).t() + Y * svdmat));
}

///////////////////////////////////////////////////////////////
///// multivariate normal density
///////////////////////////////////////////////////////////////
arma::vec dmvnrm(arma::mat x, arma::rowvec mean, arma::mat sigma, bool logd = false) { 
  using arma::uword;
  uword const n = x.n_rows, 
    xdim = x.n_cols;
  arma::vec out(n);
  // rewritten below to avoid singularity issue with chol()
  //arma::mat const rooti = arma::inv(trimatu(arma::chol(sigma)));
  mat U;
  vec d;
  mat V;
  svd(U,d,V,sigma);
  mat svdmat = (V * (matTimesVec(U.t(), sqrt(d)))).t();
  arma::mat const rooti = arma::inv(trimatu(svdmat));
  double const rootisum = arma::sum(log(rooti.diag())), 
    constants = -(double)xdim/2.0 * log2pi, 
    other_terms = rootisum + constants;
  
  arma::rowvec z;
  for (uword i = 0; i < n; i++) {
    z      = (x.row(i) - mean) * rooti;    
    out(i) = other_terms - 0.5 * arma::dot(z, z);     
  }  
  
  if (logd)
    return out;
  return exp(out);
}

// [[Rcpp::export]]
arma::mat singularwish(int df, arma::mat sigma, int dim) { 
  mat U;
  vec d;
  mat V;
  mat sigmainv = inv(sigma);
  svd(U,d,V,sigmainv);
  
  vec d_sq = sqrt(d);
  arma::mat d_sq_diag = diagmat(d_sq);
  
  NumericVector mu0(dim);
  arma::mat sigmavARM = (U * d_sq_diag * (U * d_sq_diag).t());
  NumericMatrix sigmav = wrap(sigmavARM);
  
  NumericMatrix X(df,dim);
  
  for(int i=1; i<=df; i++){  
    X(i-1,_) = mvrnorm(mu0, sigmav);
  }
  
  arma::mat Xarm = as<arma::mat> (X);
  Xarm = U * d_sq_diag * Xarm.t() * Xarm * (U * d_sq_diag).t();
  return Xarm;
}

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;
// [[Rcpp::export]]
double singulardwish(arma::mat x, int df, arma::mat sigma, bool logd=false) {
  int p = x.n_cols;
  
  if (x.n_rows != p || sigma.n_rows != p || sigma.n_cols != p) {
    Rcpp::stop("Dimensions of x and sigma must match.");
  }
  
  if (df < p) {
    Rcpp::stop("Degrees of freedom (df) must be greater than or equal to the dimension (p).");
  }
  
  arma::mat U, V;
  arma::vec s;
  arma::svd(U, s, V, sigma);
  
  arma::uvec positive_s = find(s > 1e-12); // Tolerance for numerical stability
  int rank_sigma = positive_s.n_elem;
  
  if (rank_sigma < p) {
    // Singular case: Density is defined on a lower-dimensional subspace.
    arma::mat U_pos = U.cols(positive_s);
    arma::vec s_pos = s.elem(positive_s);
    arma::mat sigma_pos = U_pos * arma::diagmat(s_pos) * U_pos.t();
    arma::mat x_pos = U_pos.t() * x * U_pos;
    
    if (arma::rank(x_pos) != rank_sigma) { // Explicitly use arma::rank
      if(logd){
        return -arma::datum::inf;
      } else {
        return 0;
      }
    }
    
    double log_det_sigma_pos = log(det(sigma_pos));
    double log_det_x_pos = log(det(x_pos));
    
    double log_const = -0.5 * df * rank_sigma * log(2.0) - 0.25 * rank_sigma * (rank_sigma - 1.0) * log(M_PI);
    
    for (int i = 1; i <= rank_sigma; ++i) {
      log_const -= 0.5 * lgamma(0.5 * (df - i + 1));
    }
    
    double log_density_val = log_const - 0.5 * df * log_det_sigma_pos + 0.5 * (df - rank_sigma - 1.0) * log_det_x_pos - 0.5 * trace(inv(sigma_pos) * x_pos);
    
    if (logd) {
      return log_density_val;
    } else {
      return exp(log_density_val);
    }
  } else {
    // Non-singular case (original calculation)
    double log_det_sigma = log(det(sigma));
    double log_det_x = log(det(x));
    
    double log_const = -0.5 * df * p * log(2.0) - 0.25 * p * (p - 1.0) * log(M_PI);
    
    for (int i = 1; i <= p; ++i) {
      log_const -= 0.5 * lgamma(0.5 * (df - i + 1));
    }
    
    double log_density_val = log_const - 0.5 * df * log_det_sigma + 0.5 * (df - p - 1.0) * log_det_x - 0.5 * trace(inv(sigma) * x);
    
    if (logd) {
      return log_density_val;
    } else {
      return exp(log_density_val);
    }
  }
}


///////////////////////////////////////////////////////////////
///// sample integer
///////////////////////////////////////////////////////////////
int sample_int(int k) {
  Rcpp::IntegerVector pool = Rcpp::seq(1, k);
  std::random_shuffle(pool.begin(), pool.end());
  return pool[0];
}

///////////////////////////////////////////////////////////////
///// Train the Polya Tree!
///////////////////////////////////////////////////////////////
// [[Rcpp::export]]
NumericMatrix ptTrain(int n, int dim, NumericMatrix z, int J){
  NumericMatrix r(n,J);
  NumericVector rfl (dim);
  double rcurr;
  
  for(int i=1; i<=n; i++){                             // For each observation i = 1, ..., n
    for(int j=J; j>=1; j--){                           // For each level j = J, ..., 1
      rfl = floor(pow(2, (j)) * pnorm( z((i-1),_) ) ); // Placement of observation i (rfl is a vector)
      rcurr = 0;
      for(int t=1; t<=dim; t++){                       // For each dimension t = 1, ..., p
        rcurr = rcurr + pow(2,(j*(t-1)))*rfl[t-1];     // Placement of observation i (rcurr is a double)
      }
      r(i-1,j-1) = rcurr;                              // Update the bin for observation i on level j
    }
  }
  return r; 
}

///////////////////////////////////////////////////////////////
///// Calculate Joint Probability of observed data
///////////////////////////////////////////////////////////////
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
//https://cran.r-project.org/web/packages/RcppDist/vignettes/RcppDist.pdf
//#include <RcppArmadillo.h>
//#include <mvnorm.h>
// [[Rcpp::export]]
double jointPRB(NumericMatrix rij, NumericMatrix z, int J, double cp, double ldet, bool ll){
  int n = z.nrow();
  int dim = z.ncol();
  double sum = 0;
  double numer;
  double denom;
  double target;
  
  NumericVector rij_tocheck;
  arma::mat zARM = as<arma::mat> (z);
  
  sum = as_scalar(dmvnrm(zARM.row(0) , mu0ARM, sigma0ARM, true));
  
  for(int i=2; i<=n; i++){   //Handle observations i=2, ..., n
    //j=1
    rij_tocheck = rij( _ , 0 );
    rij_tocheck = rij_tocheck[Range(0,(i-1)-1)];
    target = rij((i-1),0);
    numer = pow(2,dim) * cp         + pow(2,dim) * count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});   
    denom = pow(2,dim) * cp         +              (i-1);
    sum = sum+log(numer) - log(denom);
    //j=2 to j=J
    for(int j=2; j<=J; j++){
      rij_tocheck = rij( _ , (j-1));
      rij_tocheck = rij_tocheck[Range(0,(i-1)-1)];
      target = rij((i-1),(j-1));
      numer = pow(2,dim) * cp * pow(j,2) + pow(2,dim) * count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
      rij_tocheck = rij( _ , (j-1)-1);
      rij_tocheck = rij_tocheck[Range(0,(i-1)-1)];
      target = rij((i-1),(j-1)-1);
      denom = pow(2,dim) * cp * pow(j,2) +              count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
      sum = sum + log(numer) - log(denom);
    }
    //The normal part
    sum = sum + as_scalar(dmvnrm(zARM.row(i-1), mu0ARM, sigma0ARM, true));
  }
  //Return these valuse
  if(ll==true){
    return(n*ldet+sum);
  }else{
    return(exp(n*ldet+sum));
  }
}

///////////////////////////////////////////////////////////////
///// Calculate Joint Probability of observed data
///////////////////////////////////////////////////////////////
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
//https://cran.r-project.org/web/packages/RcppDist/vignettes/RcppDist.pdf
//#include <RcppArmadillo.h>
//#include <mvnorm.h>
struct add_multiple {
  int incr;
  int count;
  add_multiple(int incr)
    : incr(incr), count(0)
  {}
  inline int operator()(int d) {
    return d + incr * count++;
  }
};

double maxEl(NumericVector vec){
  int vs = vec.size();
  int out = 0;
  double res = vec[0];
  for(int i=1; i<vs; i++) {
    if(vec[i] >= res){   // >= means we grab the most normalizing maximum (if ties)
      res = vec[i];
      out = i;
    }
  }
  return out;
}

double chooseC(NumericMatrix rij, NumericMatrix z, int J, double ldet){
  NumericVector cp = {0.0009118820, 0.001905220, 0.003980628, 0.008316834, 0.01737659, 0.03630538, 0.07585381,
                      0.1584834, 0.3311237, 0.6918258, 1.445451, 3.020019, 6.309808, 13.18325, 27.54413,
                      57.54870, 120.2381, 251.2167, 524.8738, 1096.633};
  //NumericVector cp = {0.05,0.10,0.25,0.50,1.00,3.00,5.00,10.00,25.00,50.00,100.00};
  int n = z.nrow();
  int dim = z.ncol();
  NumericVector sum = NumericVector(cp.length());
  NumericVector numer;
  NumericVector denom;
  double target;
  
  NumericVector rij_tocheck;
  arma::mat zARM = as<arma::mat> (z);
  
  sum = sum + as_scalar(dmvnrm(zARM.row(0) , mu0ARM, sigma0ARM, true));
  
  for(int i=2; i<=n; i++){   //Handle observations i=2, ..., n
    //j=1
    rij_tocheck = rij( _ , 0 );
    rij_tocheck = rij_tocheck[Range(0,(i-1)-1)];
    target = rij((i-1),0);
    numer = pow(2,dim) * cp         + pow(2,dim) * count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});   
    denom = pow(2,dim) * cp         +              (i-1);
    sum = sum+log(numer/denom);
    //j=2 to j=J
    for(int j=2; j<=J; j++){
      rij_tocheck = rij( _ , (j-1));
      rij_tocheck = rij_tocheck[Range(0,(i-1)-1)];
      target = rij((i-1),(j-1));
      numer = pow(2,dim) * cp * pow(j,2) + pow(2,dim) * count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
      rij_tocheck = rij( _ , (j-1)-1);
      rij_tocheck = rij_tocheck[Range(0,(i-1)-1)];
      target = rij((i-1),(j-1)-1);
      denom = pow(2,dim) * cp * pow(j,2) +              count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
      sum = sum + log(numer/denom);
    }
    //The normal part
    sum = sum + as_scalar(dmvnrm(zARM.row(i-1), mu0ARM, sigma0ARM, true));
  }
  sum = n*ldet + sum;
  return cp[maxEl(sum)];
}

///////////////////////////////////////////////////////////////
///// Probability of Test observations
///////////////////////////////////////////////////////////////
NumericVector ptTestvec(NumericMatrix Yc, NumericVector mu, NumericMatrix sigmahalfinv, 
                        int dim, int J, double cp, NumericMatrix rij, double ldet){
  
  ////////////////////////////////////////////
  //Standardize
  ////////////////////////////////////////////
  arma::mat zARM;
  arma::mat sigmahalfinvARM = as<arma::mat> (sigmahalfinv);
  arma::mat YcARM = as<arma::mat> (Yc);
  arma::rowvec muARM = as<arma::rowvec> (mu);
  
  zARM = trans(sigmahalfinvARM * trans(YcARM.each_row()-muARM));
  
  NumericMatrix z = as<NumericMatrix>(wrap(zARM));
  
  ////////////////////////////////////////////
  //Calculate the bins for this observation
  ////////////////////////////////////////////
  NumericMatrix rnew (z.nrow(), J);
  NumericVector rfl (dim);
  double rcurr;
  
  NumericVector probabilities (z.nrow());
  NumericVector rij_tocheck;
  double target;
  double numer;
  double denom;
  double sum = 0;
  
  for(int i=1; i<=z.nrow(); i++){
    sum = 0;
    for(int j=1; j<=J; j++){                           // For each level j = J, ..., 1
      rfl = floor(pow(2,j) * pnorm(z((i-1),_) ));      // Placement of observation i (rfl is a vector)
      rcurr = 0;
      for(int t=1; t<=dim; t++){                       // For each dimension t = 1, ..., p
        rcurr = rcurr + pow(2,(j*(t-1)))*rfl[(t-1)];   // Placement of observation i (rcurr is a double)
      } 
      rnew(i-1,j-1) = rcurr;
      
      if(j>=2){
        //line 2
        rij_tocheck = rij( _ , (j-1));
        target = rnew((i-1),(j-1));
        numer = pow(2,dim) * cp * pow(j,2) + pow(2,dim) * count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
        
        rij_tocheck = rij( _ , (j-1)-1);
        target = rnew((i-1),(j-1)-1);
        denom = pow(2,dim) * cp * pow(j,2) +              count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
        
        sum = sum+log(numer/denom);    
      }
    }
    
    //line 1
    rij_tocheck = rij( _ , 0 );
    target = rnew((i-1),0);
    numer = pow(2,dim) * cp         + pow(2,dim) * count_if(rij_tocheck.begin(), rij_tocheck.end(), [&target](double i){return i == target;});
    denom = pow(2,dim) * cp         +              rij.nrow();
    sum = sum+log(numer/denom);
    
    
    //line 3
    sum = sum + as_scalar(dmvnrm(zARM.row(i-1), mu0ARM, sigma0ARM, true));
    probabilities[(i-1)] = exp(ldet+sum);
    
  }
  return probabilities;
}


///////////////////////////////////////////////////////////////
///// MCMC FUNCTION!!
///////////////////////////////////////////////////////////////
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
#include <RcppArmadillo.h>
#include <wishart.h>
// [[Rcpp::export]]
SEXP ptTest(NumericMatrix train, NumericMatrix test,
            int save, int burnin, int thin=0,int maxJ=8,
            double tunemu = 1.0, double tunesigma = 1.0, double tuneO = 1.0, double tunecp=1.0,
            bool fast = false, bool useCurrentV=true, 
            bool adaptO=true, bool adaptmu=true, bool adaptsigma=true, bool adaptcp=true,
            bool murand=true, bool sigmarand=true, 
            double csamp=1.0, // 1=MCMC, 2=PickEachIter, 3=Fixed
            double cpar1=-99.0, double cpar2=-99.0, 
            double cfix = -99.0, 
            double propaccept = 0.05,
            bool Orand=true){
  
  // A list of things to return to user
  List ret;
  int nadapt=2000;
  
  // Set up number of iterations
  int thindex =0;
  int mcmc = 0;
  int pmatupdates = 0;
  
  if(adaptO|adaptmu|adaptsigma){
    burnin = burnin + nadapt;
  }else{
    adaptO = false;
  }
  if(thin>0){
    mcmc=save*thin + burnin;
    pmatupdates = floor(0.05*save*thin);
    
  }else{
    mcmc=save + burnin;
    pmatupdates = floor(0.05*save);
  }
  
  // Placeholder for likelihoods
  double currLL;
  double propLL;
  /////////////////////////////////////////////////////////
  ///// A place to save object
  /////////////////////////////////////////////////////////
  NumericVector muaccepts (mcmc);                 // save updating result (accept=1; reject=0)
  NumericVector muacceptprbs (mcmc);              // save accept probability for rotation
  //NumericVector muvals1 (mcmc);                   // save mu values (COMMENT OUT FOR NON 2X2)
  //NumericVector muvals2 (mcmc);                   // save mu values (COMMENT OUT FOR NON 2X2)
  NumericVector sigmaaccepts (mcmc);              // save updating result (accept=1; reject=0)
  NumericVector sigmaacceptprbs (mcmc);           // save accept probability for rotation
  //NumericVector sigmavals11 (mcmc);               // save sigma values (COMMENT OUT FOR NON 2X2)
  //NumericVector sigmavals12 (mcmc);               // save sigma values (COMMENT OUT FOR NON 2X2)
  //NumericVector sigmavals21 (mcmc);               // save sigma values (COMMENT OUT FOR NON 2X2)
  //NumericVector sigmavals22 (mcmc);               // save sigma values (COMMENT OUT FOR NON 2X2)
  
  NumericVector cpaccepts (mcmc);                 // save updating result (accept=1; reject=0)
  NumericVector cpvalues (mcmc);                  // save closeness parameters (mu, sigma)
  NumericVector Oaccepts (mcmc);                  // save updating result (accept=1; reject=0)
  NumericVector Oacceptprbs (mcmc);               // save accept probability for rotation
  //NumericVector Ovals11 (mcmc);               // save O values (COMMENT OUT FOR NON 2X2)
  //NumericVector Ovals12 (mcmc);               // save O values (COMMENT OUT FOR NON 2X2)
  //NumericVector Ovals21 (mcmc);               // save O values (COMMENT OUT FOR NON 2X2)
  //NumericVector Ovals22 (mcmc);               // save O values (COMMENT OUT FOR NON 2X2)
  NumericVector mutunes (mcmc);                   // save adapative tuning
  NumericVector sigmatunes (mcmc);                // save adapative tuning
  NumericVector vtunes (mcmc);                    // save adapative tuning
  
  /////////////////////////////////////////////////////////
  ///// Initialize
  /////////////////////////////////////////////////////////
  
  /////////////////////////////////////////////////////////
  ///// Collect information about the testing and training
  /////////////////////////////////////////////////////////
  int dim = train.ncol();                                           // dimensions of observations
  int ntrain = train.nrow();                                        // n_training
  int ntest = test.nrow();                                          // n_testing
  int J = max(2, static_cast<int> (min(ceil(log(ntrain)/log(pow(2,dim))),static_cast<double>(maxJ))) );   // Choose J via Hanson's metric
  
  //standard gaussian arguments
  mu0 = NumericVector(dim);
  sigma0 = NumericMatrix(dim,dim);
  sigma0.fill_diag(1);
  mu0ARM = as<arma::rowvec> (mu0);
  sigma0ARM = as<arma::mat> (sigma0);
  
  //summary statistics 
  NumericVector datameans = colMeans(train);     // column means for each feature based on training data
  arma::rowvec datameansARM (datameans);
  NumericMatrix datacovmat = cov(train);         // covariance matrix based on training data
  arma::mat datacovmatARM = as<arma::mat>(datacovmat);
  
  
  //new for mu sampling....
  //new for mu sampling....
  arma::cx_vec eigvaldata;
  arma::cx_mat eigvecdata;
  arma::mat datacovmatzARM = (1/static_cast<double>(ntrain)) * as<arma::mat>(datacovmat);
  eig_gen(eigvaldata, eigvecdata, datacovmatzARM);
  //calculate the inverse square root
  arma::mat datacovmatzhalfinvARM = arma::conv_to<arma::mat>::from(eigvecdata*diagmat(1.0/sqrt(eigvaldata))*trans(eigvecdata));
  NumericMatrix datacovmatzhalfinv =as<NumericMatrix>(wrap(datacovmatzhalfinvARM));

  NumericVector muPROPz (dim);
  arma::rowvec muPROPzARM (dim);
  NumericVector muz (dim);
  arma::rowvec muzARM (dim);
  
  /////////////////////////////////////////////////////////
  ///// Initialize mu 
  /////////////////////////////////////////////////////////
  NumericVector mu = clone(datameans);        // column means
  arma::rowvec muARM =as<arma::rowvec> (mu); // column means
  
  
  ///// For Sampling mu 
  arma::mat sigmamuARM (dim,dim);     
  NumericMatrix sigmamu (dim,dim);
  //proposal
  NumericVector muPROP (dim);
  arma::rowvec muPROPARM (dim);
  //accept probability
  NumericMatrix zPROP (ntrain, dim);
  arma::mat zPROPARM (ntrain, dim);
  NumericMatrix rijPROP;
  double muacceptprb;
  
  /////////////////////////////////////////////////////////
  ///// Initialize sigma 
  /////////////////////////////////////////////////////////
  NumericMatrix sigma = clone(datacovmat);        // covariance matrix based on training data
  double ldet = -0.5 * getDeterminantMod(sigma);
  
  ///// For Sampling sigma 
  int df = floor(ntrain / (0.001*tunesigma));
  
  //proposal
  NumericMatrix sigmaPROP (dim,dim);
  arma::mat sigmaPROPARM (dim,dim);
  
  //accept probability
  double ldetPROP;
  arma::cx_vec eigvalPROP;
  arma::cx_mat eigvecPROP;
  double sigmaacceptprb;
  
  double cpPROP;
  
  /////////////////////////////////////////////////////////
  ///// Initialize O and sigmahalfinv
  /////////////////////////////////////////////////////////
  // initialize the v vectors (in a matrix)
  NumericMatrix Vmat (dim,dim);
  for(int j=1; j<=dim; j++){
    Vmat ((j-1),_) = mvrnorm(mu0, sigma0);
  } 
  arma::mat VmatARM = as<arma::mat> (Vmat);
  arma::mat vjvjt (dim,dim);
  double vjtvj;
  
  // Calculate the O matrix
  NumericMatrix O;
  NumericMatrix Ostart (dim,dim);
  Ostart.fill_diag(1);
  arma::mat OARM = as<arma::mat> (Ostart);
  
  if(Orand){
    for(int j=1; j<=dim; j++){
      vjvjt = trans(VmatARM.row(j-1)) * VmatARM.row(j-1);
      vjtvj = as_scalar(VmatARM.row(j-1) * trans(VmatARM.row(j-1)));
      OARM = OARM * (sigma0ARM - 2*(vjvjt/vjtvj));
    }
  }
  
  ///// For Sampling O 
  //Adaptive Sampling
  double adaptOcount=0.0;
  double adaptOrate=0.0;
  double adaptmucount=0.0;
  double adaptmurate=0.0;
  double adaptsigmacount=0.0;
  double adaptsigmarate=0.0;
  double adaptcpcount=0.0;
  double adaptcprate=0.0;
  
  NumericVector muv (dim);
  arma::vec muvARM (dim);
  arma::mat sigmavARM (dim,dim);
  NumericMatrix sigmav (dim,dim);
  //proposal
  int vtoupdate;
  NumericMatrix VmatPROP (dim,dim);
  arma::mat VmatPROPARM (dim,dim);
  arma::mat OPROPARM (dim,dim);
  NumericMatrix sigmahalfinvPROP (dim,dim);
  arma::mat sigmahalfinvPROPARM (dim, dim);  
  
  //accept probability
  double Oacceptprb;
  
  /////////////////////////////////////////////////////////
  ///// Standardize Data
  /////////////////////////////////////////////////////////
  //eigenvalue decomp
  arma::cx_vec eigval;
  arma::cx_mat eigvec;
  arma::mat sigmaARM = as<arma::mat> (sigma);
  arma::mat sigmahalfinvARM;
  NumericMatrix sigmahalfinv;
  eig_gen(eigval, eigvec, sigmaARM);
  //calculate the inverse square root
  sigmahalfinvARM = arma::conv_to<arma::mat>::from(eigvec*diagmat(1.0/sqrt(eigval))*trans(eigvec)*trans(OARM));
  sigmahalfinv =as<NumericMatrix>(wrap(sigmahalfinvARM));
  
  arma::mat zARM;
  arma::mat trainARM = as<arma::mat> (train);
  zARM = trans(sigmahalfinvARM * trans(trainARM.each_row()-muARM));
  NumericMatrix z = as<NumericMatrix>(wrap(zARM));
  
  /////////////////////////////////////////////////////////
  ///// This part trains the Polya tree
  /////////////////////////////////////////////////////////  
  NumericMatrix rij;
  rij = ptTrain(ntrain,dim,z,J);
  
  /////////////////////////////////////////////////////////
  ///// Initialize cp
  /////////////////////////////////////////////////////////
  double cp; 
  if(csamp == 3.0){
    if(cfix>0){
      cp = cfix;
      Rcout << "\nThe closeness parameter is fixed and set to " << cp << ".\n";
    }else{
      cp = 1;
      Rcout << "\nThe closeness parameter is fixed and set to 1.\n";
    }
  }else{
    cp = chooseC(rij    , z    , J, ldet);
    Rcout << "\nThe closeness parameter is initialized to " << cp << ".\n";
  }
  
  // select cpars if is random and cpars aren't specified
  if((csamp == 1.0) & ((cpar1 < 0) | (cpar2 <0))){
    cpar1 = chooseC(rij    , z    , J, ldet);
    cpar2 = 1.0;
    Rcout << "The closeness hyperparameters are initialized to (" << cpar1 << "," << cpar2 << ").";
  }else{
    Rcout << "The closeness hyperparameters are initialized to (" << cpar1 << "," << cpar2 << ").";
  }	
  
  /////////////////////////////////////////////////////////
  ///// START MCMC
  /////////////////////////////////////////////////////////
  //updates
  int bigupdates = floor(0.10*mcmc);
  int updates = floor(0.01*mcmc);
  double numerator;
  double denominator;
  //double U;
  bool updated=true;
  
  //calculating density
  NumericMatrix probabilitymat (ntest,20);
  NumericVector probabilityvec (ntest);
  arma::rowvec probabilityvecARM (ntest);
  NumericVector probabilityvecCURR (ntest);
  NumericVector rotWeights (floor(save));
  arma::rowvec rotWeightsARM (floor(save));
  
  double denom;
  
  currLL = jointPRB(rij    , z    , J, cp, ldet, true);
  
  Rcout << "\nStarting MCMC...\n";
  Rcout << "Burnin: Iteration " << "0" << " / " << mcmc << "";
  
  for(int i=1; i<=mcmc; i++){
    /////////////////////////////////////////////////////////
    ///// Update Progress
    /////////////////////////////////////////////////////////
    if((i % bigupdates == 0) & (i != mcmc)){
      if(i<burnin){
        Rcout << "-\n"<<"Burnin: Iteration " << i << " / " << mcmc << "";
      }else{
        Rcout << "-\n"<<"Sampling: Iteration " << i << " / " << mcmc << "";  
      }
    }
    if((i % updates == 0) & (i != mcmc)){
      Rcout << "-";
    }
    /////////////////////////////////////////////////////////
    ///// Update mu
    /////////////////////////////////////////////////////////
    if(murand){
      if(adaptmu & (i%100==0)){
        //Reset tracking
        adaptmurate=adaptmucount/(1.0*100);
        adaptmucount=0;
        
        if(i < 2000){
          if(adaptmurate<0.234){
            tunemu=exp(log(tunemu)-(0.234-adaptmurate));
          }else{
            tunemu=exp(log(tunemu)+(adaptmurate-0.234));
          }
        }else{
          if(adaptmurate<0.234){
            tunemu=exp(log(tunemu)-min(0.01,1.0/i));
          }else{
            tunemu=exp(log(tunemu)+min(0.01,1.0/i));
          }
        }
      }
      mutunes[i-1] = tunemu;
      sigmamuARM = (tunemu/ntrain)*sigmaARM;
      sigmamu = as<NumericMatrix>(wrap(sigmamuARM));
      muPROP = mvrnorm(mu, sigmamu);
      muPROPARM = as<arma::rowvec> (muPROP);
      
      muPROPzARM = trans(datacovmatzhalfinvARM * trans(muPROPARM.each_row()-datameansARM));
      muPROPz=as<NumericVector>(wrap(muPROPzARM)); 
      
      // Standardize Data
      zPROPARM = trans(sigmahalfinvARM * trans(trainARM.each_row()-muPROPARM));
      zPROP = as<NumericMatrix>(wrap(zPROPARM));
      // This part trains the Polya tree
      rijPROP = ptTrain(ntrain,dim,zPROP,J);
      // Updating
      propLL = jointPRB(rijPROP, zPROP, J, cp, ldet, true);
      
      numerator   = propLL + as_scalar(dmvnrm(muPROPzARM,  mu0ARM, sigma0ARM, true)) + as_scalar(dmvnrm(muARM,     muPROPARM, sigmamuARM, true));
      denominator = currLL + as_scalar(dmvnrm(muzARM,     mu0ARM, sigma0ARM, true)) + as_scalar(dmvnrm(muPROPARM, muARM, sigmamuARM, true));
      
      
      muacceptprb = exp(numerator-denominator);
      muacceptprbs[i-1]=muacceptprb;
      
      if(muacceptprb > as<double>(runif(1,0,1))){
        updated = true;
        currLL = propLL;
        muaccepts[i-1] = 1;
        mu = clone(muPROP);
        muARM = muPROPARM;
        z=clone(zPROP);
        zARM = zPROPARM;
        rij=clone(rijPROP);
        
        adaptmucount=adaptmucount+1;
        
        muz=clone(muPROPz);
        muzARM=muPROPzARM;
      }
      
      //muvals1[i-1] = mu[0]; //(COMMENT OUT FOR NON 2X2)
      //muvals2[i-1] = mu[1]; //(COMMENT OUT FOR NON 2X2)
    }
    /////////////////////////////////////////////////////////
    ///// Update sigma
    /////////////////////////////////////////////////////////
    if(sigmarand){
      if(adaptsigma & (i%100==0)){
        //Reset tracking
        adaptsigmarate=adaptsigmacount/(1.0*100);
        adaptsigmacount=0;
        
        if(i < 2000){
          if(adaptsigmarate<0.234){
            tunesigma=exp(log(tunesigma)-(0.234-adaptsigmarate));
          }else{
            tunesigma=exp(log(tunesigma)+(adaptsigmarate-0.234));
          }
        }else{
          if(adaptsigmarate<0.234){
            tunesigma=exp(log(tunesigma)-min(0.01,1.0/i));
          }else{
            tunesigma=exp(log(tunesigma)+min(0.01,1.0/i));
          }
        }
      }
      sigmatunes[i-1] = tunesigma;
      df = floor(ntrain / (0.001*tunesigma));
      
      //update sigma
      sigmaPROPARM = singularwish(df,(df-dim-1)*sigmaARM, dim);
      
      sigmaPROP=as<NumericMatrix>(wrap(sigmaPROPARM));
      ldetPROP = -0.5 * getDeterminantMod(sigmaPROP);
      
      eig_gen(eigvalPROP, eigvecPROP, sigmaPROPARM);
      
      //calculate the inverse square root
      if(Orand){
        sigmahalfinvPROPARM = arma::conv_to<arma::mat>::from(eigvecPROP*diagmat(1.0/sqrt(eigvalPROP))*trans(eigvecPROP)*trans(OARM));
      }else{
        sigmahalfinvPROPARM = arma::conv_to<arma::mat>::from(eigvecPROP*diagmat(1.0/sqrt(eigvalPROP))*trans(eigvecPROP));
      }
      sigmahalfinvPROP = as<NumericMatrix>(wrap(sigmahalfinvPROPARM));
      // Standardize Data
      zPROPARM = trans(sigmahalfinvPROPARM * trans(trainARM.each_row()-muARM));
      zPROP = as<NumericMatrix>(wrap(zPROPARM));
      // This part trains the Polya tree
      rijPROP = ptTrain(ntrain,dim,zPROP,J);
      propLL = jointPRB(rijPROP, zPROP, J, cp, ldetPROP, true);
      // Updating
      numerator   = propLL +  as_scalar(singulardwish(sigmaPROPARM, ntrain,(ntrain-dim-1)*datacovmatARM,true)) + as_scalar(singulardwish(sigmaARM,      df,(df-dim-1)*sigmaPROPARM,    true));
      denominator = currLL +  as_scalar(singulardwish(sigmaARM, ntrain,(ntrain-dim-1)*datacovmatARM,true)) + as_scalar(singulardwish(sigmaPROPARM, df,(df-dim-1)*sigmaARM,        true));
      
      sigmaacceptprb = exp(numerator-denominator);
      sigmaacceptprbs[i-1]=sigmaacceptprb;
      
      if(sigmaacceptprb > as<double>(runif(1,0,1))){
        updated = true;
        currLL = propLL;
        sigmaaccepts[i-1] = 1;
        sigma = clone(sigmaPROP);
        sigmaARM = sigmaPROPARM;
        ldet = ldetPROP;
        eigval=eigvalPROP;
        eigvec=eigvecPROP;
        z=clone(zPROP);
        zARM = zPROPARM;
        rij=clone(rijPROP);
        sigmahalfinv=clone(sigmahalfinvPROP);
        sigmahalfinvARM=sigmahalfinvPROPARM;
        adaptsigmacount=adaptsigmacount+1;
      }
      //sigmavals11[i-1] =  sigmaARM(0,0);               // save sigma values (COMMENT OUT FOR NON 2X2)
      //sigmavals12[i-1] =  sigmaARM(0,1);               // save sigma values (COMMENT OUT FOR NON 2X2)
      //sigmavals21[i-1] =  sigmaARM(1,0);               // save sigma values (COMMENT OUT FOR NON 2X2)
      //sigmavals22[i-1] =  sigmaARM(1,1);               // save sigma values (COMMENT OUT FOR NON 2X2)
    }
    ///////////////////////////////////////////////////////
    /// Update cp
    ///////////////////////////////////////////////////////
    if(csamp==1.0){
      if((i%100==0)){
        //Reset tracking
        adaptcprate=adaptcpcount/(1.0*100);
        adaptcpcount=0;

        if(i < 2000){
          if(adaptcprate<0.44){
            tunecp=exp(log(tunecp)-(0.44-adaptcprate));
          }else{
            tunecp=exp(log(tunecp)+(adaptcprate-0.44));
          }
        }else{
          if(adaptsigmarate<0.44){
            tunecp=exp(log(tunecp)-min(0.01,1.0/i));
          }else{
            tunecp=exp(log(tunecp)+min(0.01,1.0/i));
          }
        }
      }

      cpPROP = as<double>(rlnorm(1, log(cp), tunecp));

      // Updating
      propLL = jointPRB(rij, z, J, cpPROP, ldet, true);
      numerator   = propLL + R::dgamma(cpPROP, cpar1, cpar2, false) + R::dlnorm(cp, log(cpPROP), tunecp, false);
      denominator = currLL + R::dgamma(cp, cpar1, cpar2, false)  + R::dlnorm(cpPROP, log(cp), tunecp, false);

      if(exp(numerator-denominator) > as<double>(runif(1,0,1))){
        updated = true;
        currLL = propLL;
        cpaccepts[i-1] = 1;
        cp = cpPROP;
        adaptcpcount=adaptcpcount+1;
      }
    }else{
      if(csamp==2.0){
        cp = chooseC(rij    , z    , J, ldet);
      }
    }
    cpvalues[i-1] = cp;
    
    
    /////////////////////////////////////////////////////////
    ///// update a v,H, and O
    /////////////////////////////////////////////////////////
    if(Orand){
      vtoupdate =  sample_int(dim);
      if(useCurrentV){
        if(adaptO & (i%100==0)){
          //Reset tracking
          adaptOrate=adaptOcount/(1.0*100);
          adaptOcount=0;
          
          if(i < 2000){
            if(adaptOrate<0.234){
              tuneO=exp(log(tuneO)-(0.234-adaptOrate));
            }else{
              tuneO=exp(log(tuneO)+(adaptOrate-0.234));
            }
          }else{
            if(adaptOrate<0.234){
              tuneO=exp(log(tuneO)-min(0.01,1.0/i));
            }else{
              tuneO=exp(log(tuneO)+min(0.01,1.0/i));
            }
          }
        }
        vtunes[i-1] = tuneO;
        VmatPROP = clone(Vmat);
        muv = Vmat((vtoupdate-1),_);
        sigmavARM = as<arma::mat> (sigma0);
        sigmavARM = (tuneO*0.005)*sigmavARM;
        sigmav = as<NumericMatrix>(wrap(sigmavARM));
        VmatPROP ((vtoupdate-1),_) = mvrnorm(muv, sigmav); 
      }else{
        vtunes[i-1] = tuneO;
        VmatPROP ((vtoupdate-1),_) = mvrnorm(mu0, sigma0); 
      }
      OPROPARM = as<arma::mat> (Ostart);
      VmatPROPARM = as<arma::mat> (VmatPROP);
      for(int j=1; j<=dim; j++){
        vjvjt = trans(VmatPROPARM.row(j-1)) * VmatPROPARM.row(j-1);
        vjtvj = as_scalar(VmatPROPARM.row(j-1) * trans(VmatPROPARM.row(j-1)));
        OPROPARM = OPROPARM * (sigma0ARM - 2*(vjvjt/vjtvj));
      }
      
      //calculate the inverse square root
      sigmahalfinvPROPARM = arma::conv_to<arma::mat>::from(eigvec*diagmat(1.0/sqrt(eigval))*trans(eigvec)*trans(OPROPARM));
      sigmahalfinvPROP = as<NumericMatrix>(wrap(sigmahalfinvPROPARM));
      
      // Standardize Data
      zPROPARM = trans(sigmahalfinvPROPARM * trans(trainARM.each_row()-muARM));
      zPROP = as<NumericMatrix>(wrap(zPROPARM));
      
      // This part trains the Polya tree
      rijPROP = ptTrain(ntrain,dim,zPROP,J);
      
      // Updating
      propLL = jointPRB(rijPROP, zPROP, J, cp, ldet, true);
      
      numerator   = propLL + as_scalar(dmvnrm(VmatPROPARM.row(vtoupdate-1), mu0ARM, sigma0ARM, true)) + as_scalar(dmvnrm(VmatARM.row(vtoupdate-1), VmatPROPARM.row(vtoupdate-1), sigmavARM, true));
      denominator = currLL + as_scalar(dmvnrm(VmatARM.row(vtoupdate-1), mu0ARM, sigma0ARM, true)) + as_scalar(dmvnrm(VmatPROPARM.row(vtoupdate-1), VmatARM.row(vtoupdate-1), sigmavARM, true));
      Oacceptprb = exp(numerator-denominator);
      Oacceptprbs[i-1]=Oacceptprb;
      
      if(Oacceptprb > as<double>(runif(1,0,1))){
        updated = true;
        currLL = propLL;
        Oaccepts[i-1] = 1;
        Vmat = clone(VmatPROP);
        VmatARM = VmatPROPARM;
        O=as<NumericMatrix>(wrap(OPROPARM));
        OARM = OPROPARM;
        z=clone(zPROP);
        zARM=zPROPARM;
        rij=clone(rijPROP);
        sigmahalfinv=clone(sigmahalfinvPROP);
        sigmahalfinvARM=sigmahalfinvPROPARM;
        adaptOcount=adaptOcount+1;
      }else if(fast){ // Checks if the rotations should be saved randomly when FAST 
        if(propaccept > as<double>(runif(1,0,1))){
          updated = true;
          currLL = propLL;
          Oaccepts[i-1] = 1;
          Vmat = clone(VmatPROP);
          VmatARM = VmatPROPARM;
          O=as<NumericMatrix>(wrap(OPROPARM));
          OARM = OPROPARM;
          z=clone(zPROP);
          zARM=zPROPARM;
          rij=clone(rijPROP);
          sigmahalfinv=clone(sigmahalfinvPROP);
          sigmahalfinvARM=sigmahalfinvPROPARM;
        }
      }
      //Ovals11[i-1] = O(0,0);// save O values (COMMENT OUT FOR NON 2X2)
      //Ovals12[i-1] = O(0,1);// save O values (COMMENT OUT FOR NON 2X2)
      //Ovals21[i-1] = O(1,0);// save O values (COMMENT OUT FOR NON 2X2)
      //Ovals22[i-1] = O(1,1);// save O values (COMMENT OUT FOR NON 2X2)
    }
    /////////////////////////////////////////////////////////
    ///// Evaluate Probabilities for Testing Data
    /////////////////////////////////////////////////////////
    if(i > burnin){
      if(thin==0){
        if(updated){ // there has been an update so recalculate probability
          rotWeights[(i-burnin)-1] = 1.0/(floor(save));
          probabilityvecCURR = rotWeights[(i-burnin)-1] * ptTestvec(test, mu, sigmahalfinv, dim, J, cp, rij, ldet);
          probabilityvec = probabilityvec + probabilityvecCURR;
          updated=false;
        }else{ //there has been no update so copy over previous run
          rotWeights[(i-burnin)-1] =  rotWeights[(i-burnin)-1-1];
          probabilityvec = probabilityvec + probabilityvecCURR;
        }
        if(((i-burnin) % pmatupdates) == 0){
          rotWeightsARM = as<arma::rowvec> (rotWeights);
          denom = sum(rotWeightsARM);
          probabilityvecARM= as<arma::rowvec> (probabilityvec);
          probabilityvecARM= probabilityvecARM/denom;
          
          probabilitymat(_,floor((i-burnin)/pmatupdates)-1) = as<NumericVector>(wrap(probabilityvecARM));
        }
      }else{
        if(((i-burnin) % thin) == 0){
          if(updated){ // there has been an update so recalculate probability
            rotWeights[(i-burnin)/thin-1] = 1.0/(floor(save));
            probabilityvecCURR = rotWeights[(i-burnin)/thin-1] * ptTestvec(test, mu, sigmahalfinv, dim, J, cp, rij, ldet);
            probabilityvec = probabilityvec + probabilityvecCURR;
            updated=false;
          }else{ //there has been no update so copy over previous run
            rotWeights[(i-burnin)/thin-1] =  rotWeights[(i-burnin)/thin-1-1];
            probabilityvec = probabilityvec + probabilityvecCURR;
          }
          if(((i-burnin) % pmatupdates) == 0){
            rotWeightsARM = as<arma::rowvec> (rotWeights);
            denom = sum(rotWeightsARM);
            probabilityvecARM= as<arma::rowvec> (probabilityvec);
            probabilityvecARM= probabilityvecARM/denom;
            
            probabilitymat(_,thindex) = as<NumericVector>(wrap(probabilityvecARM));
            thindex= thindex+1;
          }
        }
      }
    }
  }
  // END MCMC
  ret["pmat"] = probabilitymat;
  
  Rcout << "-\n"<<"Wrapping up MCMC output...\n"; 
  
  rotWeightsARM = as<arma::rowvec> (rotWeights);
  denom = sum(rotWeightsARM);
  rotWeightsARM = rotWeightsARM/denom;
  rotWeights = as<NumericVector>(wrap(rotWeightsARM));
  //vector approach
  probabilityvecARM= as<arma::rowvec> (probabilityvec);
  probabilityvecARM= probabilityvec/denom;
  probabilityvec = as<NumericVector>(wrap(probabilityvecARM));
  ret["pvec"] = probabilityvec;
  
  
  ret["rotWeights"] = rotWeights;
  if(murand){
    ret["mutune"]=mutunes;
    ret["muaccepts"] = muaccepts;
    ret["muacceptprbs"] = muacceptprbs;
    //ret["muvals1"] = muvals1;// save mu values (COMMENT OUT FOR NON 2X2)
    //ret["muvals2"] = muvals2;// save mu values (COMMENT OUT FOR NON 2X2)
  }
  if(sigmarand){
    ret["sigmatune"]=sigmatunes;
    ret["sigmaaccepts"] = sigmaaccepts;
    ret["sigmaacceptprbs"] = sigmaacceptprbs; 
    //ret["sigmavals11"] = sigmavals11;// save sigma values (COMMENT OUT FOR NON 2X2)
    //ret["sigmavals12"] = sigmavals12;// save sigma values (COMMENT OUT FOR NON 2X2)
    //ret["sigmavals21"] = sigmavals21;// save sigma values (COMMENT OUT FOR NON 2X2)
    //ret["sigmavals22"] = sigmavals22;// save sigma values (COMMENT OUT FOR NON 2X2)
  }
  
  if(csamp==1){
    ret["cpaccepts"] = cpaccepts;
  }
  
  ret["cpvalues"] = cpvalues;
  ret["Oaccepts"] = Oaccepts;
  ret["Oacceptprbs"] = Oacceptprbs;
  //ret["Ovals11"] = Ovals11;// save O values (COMMENT OUT FOR NON 2X2)
  //ret["Ovals12"] = Ovals12;// save O values (COMMENT OUT FOR NON 2X2)
  //ret["Ovals21"] = Ovals21;// save O values (COMMENT OUT FOR NON 2X2)
  //ret["Ovals22"] = Ovals22;// save O values (COMMENT OUT FOR NON 2X2)
  ret["vtune"]=vtunes;
  return ret;
}

