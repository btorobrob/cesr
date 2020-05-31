#include <Rcpp.h>
using namespace Rcpp;

// Create capture histories for each individual, with two periods for first year

// [[Rcpp::export]]
StringVector createCH(IntegerMatrix caps, IntegerMatrix cov) {
  
  int i, j;
  int nind = caps.nrow();
  int nyrs = caps.ncol();
  int first = 0;
  StringVector ch(nind);
  String tmp;
  
  for( i=0; i<nind; ++i ){
    tmp = "";                                // to gather the capture events
    
    for( j=0; j<nyrs; ++j ){                 // get the year of first capture
      if( caps(i, j) > 0 ){                  // (sl. faster than using a while loop)
        first = j;
        break; 
      } //endif
    } // j

    if( first > 0) {                        // fill in the pre-captures...
      for( j=0; j<first; ++j ){
        if( cov(i,j) )
          tmp = tmp.push_back("0");         // ... site operating
        else
          tmp = tmp.push_back(".");         // ... site not operating
      } // j
    } // endif
    
    if( caps(i, first) == 1)                // the capture year
      tmp = tmp.push_back("10");            // not caught again in the first year
    else
      tmp = tmp.push_back("11");            // caught twice in the first year
    
    if( first < nyrs ) {                    // fill in the post-captures...
      for( j=(first+1); j<nyrs; ++j ){       
        if( cov(i, j) ){                    // ... site operating
          if( caps(i, j) == 2 )
            tmp = tmp.push_back("1");       // .. ignore the subsequent caught twices 
          else
            tmp = tmp.push_back(caps(i,j)); 
        } // endif 
        else
          tmp = tmp.push_back(".");         // site not operating
      } // j
    } // endif
    ch[i] = tmp;
  } // i
  return(ch);
}    
  
 