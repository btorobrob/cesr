# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

calcNprime <- function(capsdf, missdf, sitedf) {
    .Call('_cesr_calcNprime', PACKAGE = 'cesr', capsdf, missdf, sitedf)
}

createCH <- function(caps, cov) {
    .Call('_cesr_createCH', PACKAGE = 'cesr', caps, cov)
}

