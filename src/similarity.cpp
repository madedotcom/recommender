#include <math.h>
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

//' Calculates cosine similarity matrix for a given matix
//'
//' @description Tranforms product hits matrix to product similarity matrix
//' @param x matrix of prouct hits
//' @return matrix of product similarity
// [[Rcpp::export]]
arma::mat cosineCpp(arma::mat x) {

  // calculate cross product.
  arma::mat crossprod = arma::trans(x) * x;

  // calculate cosine similarity.
  int i, j, n;
  n = x.n_rows;
  arma::mat res(n, n);
  for(i = 0 ; i < n; i++) {
    for(j = 0; j <= i; j++) {
      res(i, j) = crossprod(i, j) / sqrt(crossprod(i, i) * crossprod(j, j));
      res(j, i) = res(i, j);
    }
  }
  return(res);
}
