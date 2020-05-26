#include <Rcpp.h>
using namespace Rcpp;

// Calculate for Nprime for each site and year: the number of birds that would have been caught (across all years) 
// given the visits missing in that year (i.e how many individuals were caught only on those visits?) 

// [[Rcpp::export]]
IntegerMatrix calcNprime(DataFrame capsdf, DataFrame missdf, DataFrame sitedf) {

  // First unpack the DataFrames into vectors
  IntegerVector c_site = capsdf["site"];
  IntegerVector c_year = capsdf["year"];
  IntegerVector c_visit = capsdf["visit"];
  NumericVector c_ring = capsdf["ring"];

  IntegerVector m_site = missdf["site"];
  IntegerVector m_year = missdf["year"];
  IntegerVector m_visit = missdf["visit"];
  
  IntegerVector sites = sitedf["site"];
  IntegerVector siten = sitedf["ncaps"]; 

  // figure out the vector/array dimensions
  int nrows = c_site.size();
  int nsites = sites.size();
  int fy = min(c_year);
  int ly = max(c_year);
  int nyears = ly - fy + 1;

  // create some vectors and a matrix to hold the results  
  IntegerVector misvis(20); // for the missing visits in any one year
  LogicalVector valid_visit(nrows); // which of the visits in d1_visit are valid
  LogicalVector valid_row(nrows); // which of the rows in d1_ring are valid
  NumericVector valid_inds(nrows); // an intermediate vector that seems to help 
  IntegerMatrix res(nsites, nyears); // a matrix to hold the results

  int col = 0; // initialise the column selector

  for( int i=0; i<nsites; ++i ){
    for( int j=fy; j<=ly; ++j ){
      col = j - fy; // index the column back zero
      misvis = 0;
      misvis = m_visit[(m_site==sites[i]) & (m_year==j)]; // get the missing visits this year
      if( misvis.size() > 0 ) // there are some
        valid_visit = is_na(match(c_visit, misvis));
      else // all visits are valid so just create a vector of TRUEs
        valid_visit = rep(TRUE, nrows);
      if( siten[i] != 0 ) // enough birds for a site specific correction factor
        valid_row = (c_site==sites[i]) & valid_visit; // birds caught on this site (in any year) on a valid visit
      else
        valid_row = valid_visit; // use all sites when there are very few captures
      valid_inds = c_ring[valid_row];  // it complained if I did this within unique()
      res(i, col) = unique(valid_inds).size(); // the number of unique individuals
    }
  }
  
  return(res);

}
