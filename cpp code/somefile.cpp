// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]
#include <RcppNumerical.h>


using namespace Numer;


// Define the weight function, Gaussian mixture function
class GaussianMixture: public Func
{
private:
  const double p;
  const double mu1;
  const double sigma1;
  const double mu2;
  const double sigma2;
public:
  GaussianMixture(double p_, double mu1_, double sigma1_,double mu2_, double sigma2_) : p(p_), mu1(mu1_), sigma1(sigma1_), mu2(mu2_), sigma2(sigma2_) {}
  double operator()(const double& x) const
  { 
    double res = (p * R::dnorm(x, mu1, sigma1, FALSE)) + ((1 - p) * R::dnorm(x, mu2, sigma2, FALSE));
    return  res;
  }
};
// [[Rcpp::export]]
double gm_intg(double p, double mu1, double sigma1, double mu2, double sigma2, double lower, double upper)
{
  GaussianMixture f(p, mu1, sigma1, mu2, sigma2);
  double err_est;
  int err_code;
  return integrate(f, lower, upper, err_est, err_code);
}