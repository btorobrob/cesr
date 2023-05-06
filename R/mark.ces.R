mark.ces <-
function(cesobj, exclude=NULL, type='+', trend=0, constant=0, compare=0, cleanup=TRUE){
  
  if( class(cesobj)[1]!="ces" | class(cesobj)[2]!='ch' )
    stop('use extract.ch() to create a Mark data list')
  
  if( type != '+' & type != ':' )
    stop('type should be either "+" or ":"')

  if( sum(trend>0, compare>0, constant>0) > 1 )
    stop('Specify only one of trend, compare, constant')

  # create a directory for the markfiles
  oldwd <- getwd()
  if( dir.exists('./markfiles') )
    setwd('./markfiles')
  else if( length(grep('markfiles', getwd())) > 0 ){} # don't do this recursively!
  else if( dir.create('./markfiles', showWarnings=FALSE) == FALSE ){
    dir.create('~/markfiles', showWarnings=FALSE)
    setwd('~/markfiles')
  } else 
    setwd('./markfiles') # because dir.create() will have been successful
  on.exit(setwd(oldwd))

  chdata <- cesobj$chdata
  chdata$sitename <- as.factor(chdata$sitename)
  
  if( !is.null(exclude) ){
    excl.rows <- which(as.character(chdata$sitename) %in% as.character(exclude))
    chdata <- chdata[-excl.rows, ]
    chdata$sitename <- droplevels(chdata$sitename)
  }
  if( length(levels(chdata$sitename)) == 1 ) # check if only 1 site
    groups <- cesobj$group$name
  else 
    groups <- c("sitename", cesobj$group$name)

  groups <- groups[!is.na(groups)]   # cesobj$group will be NA if group isn't set
  
  nosite <- FALSE
  if( length(groups) == 0 ){
    x.pd <- RMark::process.data(chdata, begin.time=cesobj$begin.time)
    nosite <- TRUE # only 1 site so cant fit this factor
  } else 
    x.pd <- RMark::process.data(chdata, begin.time=cesobj$begin.time, groups=groups)
  
  ddl <- RMark::make.design.data(x.pd, parameters=list(Phi=list(age.bins=c(0,1,100)),
                                                       p=list(age.bins=c(0,1,100))))
  # Separate out the initial capture period
  # the years are associated w/ time periods when phi_res is constructed
  ddl$Phi$time_var <- as.factor(ifelse(ddl$Phi$Cohort==ddl$Phi$Time, 1, ddl$Phi$time)) 
  # reverse the age factor, so we estimate within season retrap prob separately for each site
  ddl$p$age <- as.numeric(as.numeric(ddl$p$age) == 1) 
  
  ## now setup the models
  if( trend > 0 ){
    trend <- floor(trend) # just in case
    # work out Time period to start the trend
    nyrs <- cesobj$years - ifelse(trend > cesobj$years, cesobj$years, trend) + 1 
    ddl$Phi$Tind <- ifelse(ddl$Phi$Time >= nyrs, 1, 0) # years with a trend 
    ddl$Phi$tind <- 1 - ddl$Phi$Tind       # years before trend
    ddl$Phi$tind[ddl$Phi$time_var==1] <- 1 # make sure the transient period is picked up
    ddl$Phi$Tind[ddl$Phi$tind==1] <- 0     # but exclude transient period from trend
    if( is.na(cesobj$group$name) )
      phi.ces <- list(formula = as.formula('~tind:time_var+Tind:Time'))
    else
      phi.ces <- list(formula = as.formula(paste0('~', cesobj$group$name, '+tind:time_var+', cesobj$group$name, ':Tind:Time')))
    model.name <- 'trend'
    model.yrs <- ifelse(trend > cesobj$years, cesobj$years, trend)
  } else if( constant > 0 ){
    constant <- floor(constant) # just in case
    nyrs <- cesobj$years - ifelse(constant > cesobj$years, cesobj$years, constant) + 1 
    ddl$Phi$Cind <- ifelse(ddl$Phi$Time >= nyrs, 1, 0) # years within compare period 
    ddl$Phi$tind <- 1 - ddl$Phi$Cind       # years before compare
    ddl$Phi$tind[ddl$Phi$time_var==1] <- 1 # make sure the transient year is picked up
    ddl$Phi$Cind[ddl$Phi$tind==1] <- 0     # but exclude transient period from compare period
    if( is.na(cesobj$group$name) )
      phi.ces <- list(formula = as.formula('~tind:time_var+Cind'))
    else
      phi.ces <- list(formula = as.formula(paste0('~', cesobj$group$name, '+tind:time_var+', cesobj$group$name, ':Cind')))
    model.name <- 'constant'
    model.yrs <- ifelse(compare > cesobj$years, cesobj$years, constant)
  } else if( compare > 0 ){
    compare <- floor(compare) # just in case
    nyrs <- cesobj$years - ifelse(compare > cesobj$years, cesobj$years, compare) 
    ddl$Phi$Cind <- ifelse(ddl$Phi$Time >= nyrs, 1, 0) # years within compare period 
    ddl$Phi$Cind[ddl$Phi$Time >= cesobj$years] <- 0
    ddl$Phi$tind <- 1 - ddl$Phi$Cind       # years before compare
    ddl$Phi$tind[ddl$Phi$time_var==1] <- 1 # make sure the transient year is picked up
    ddl$Phi$Cind[ddl$Phi$tind==1] <- 0     # but exclude transient period from compare period
    if( is.na(cesobj$group$name) )
      phi.ces <- list(formula = as.formula('~tind:time_var+Cind'))
    else
      phi.ces <- list(formula = as.formula(paste0('~', cesobj$group$name, '+tind:time_var+', cesobj$group$name, ':Cind')))
    model.name <- 'compare'
    model.yrs <- ifelse(compare > cesobj$years, cesobj$years, compare)
  } else {
    if( is.na(cesobj$group$name) )
      phi.ces <- list(formula = as.formula('~time_var'))
    else
      phi.ces <- list(formula = as.formula(paste('~', paste('time_var', cesobj$group$name, sep=type))))    
    model.name <- 'annual'
    model.yrs <- 0
  }

  if( nosite )
    p.ces <- list(formula=~age)
  else 
    p.ces <- list(formula=~sitename*age)
  
  ## Now run the MARK models
  model.ces <- RMark::make.mark.model(x.pd, ddl, parameters=list(Phi=phi.ces,p=p.ces))
  model <- suppressWarnings(RMark::run.mark.model(model.ces, delete=cleanup, invisible=TRUE, ignore.stderr=TRUE))
  # now complains about embedded NULs not sure why
  
  # and possibly reset working dir
  if( cleanup == FALSE )
    setwd(oldwd)

  phi_indices <- grep('Phi', rownames(model$results$real))
  phi_res <- model$results$real[phi_indices, ]
  
  rrows <- grep("a0", rownames(phi_res))
  r_res <- phi_res[rrows, 1:4]
  s_res <- phi_res[-(rrows), 1:4]
  s_res$rank <- rank(-s_res$estimate)
  
  if( !is.na(cesobj$group$name) ){
    r_res$group <- cesobj$group$levels
    r_res <- r_res[ , c(5,1:4)]
    s_res$group <- rep(cesobj$group$levels, each=cesobj$years)
    s_res$years <- rep(c(cesobj$begin.time:(cesobj$begin.time+cesobj$years-1)),length(cesobj$group$levels))  
    s_res <- s_res[ , c(6,1:5)]
  } else {
    if( constant > 0 ) # replicate the last row to generate enough annual estimates
      s_res <- s_res[c(1:nrow(s_res), rep(nrow(s_res), model.yrs-1)), ]
    else if( compare > 0 ){
      nn <- nyrs 
      s_res <- s_res[c(1:nn, rep(nn,model.yrs-1), nrow(s_res)), ]
    }
    s_res$years <- c(cesobj$begin.time:(cesobj$begin.time+cesobj$years-1))  
    s_res <- s_res[ , c(6,1:5)]
  }
  rownames(r_res) <- NULL
  rownames(s_res) <- NULL
  
  p0_indices <- grep('p',rownames(model$results$real))
  p0_res <- model$results$real[p0_indices,]
  # the site specific recaptures
  p_indices <- grep('a1',rownames(p0_res))
  p_res <- p0_res[p_indices,1:4]
  sites <- as.character(t(as.data.frame(strsplit(rownames(p_res),' '))[2,]))
  sites <- as.numeric(gsub("[[:alpha:]]", '', sites)) # remove any alpha chars, hopefully groupnames don't contain numbers!
  p_res$sitename <- sites
  p_res <- p_res[ , c(5,1:4)]
  rownames(p_res) <- NULL
  # the site specific first-year recaptures
  p1_indices <- grep('a2',rownames(p0_res))
  p1_res <- p0_res[p1_indices, (1:4)]
  p1_res$sitename <- sites
  p1_res <- p1_res[ , c(5,1:4)]
  rownames(p1_res) <- NULL
  
  # check to see whether there are any boundary estimates ...
  low.p <- (p_res$estimate < 0.01)
  if( sum(low.p) > 0 ){
    if( sum(low.p < 10) )
      warning(paste("some sites have near zero recapture probabilities:", paste(p_res$sitename[low.p], collapse=",")), call.=FALSE)
    else
      warning("more than ten sites have near zero recapture probabilities", call.=FALSE)
  }
  # ... unlikely to be any high ones, but just in case 
  high.p <- (p_res$estimate > 0.95)
  if( sum(high.p) > 0 )
    warning(paste("some sites have improbably high recapture probabilities:", paste(p_res$sitename[high.p], collapse=",")), call.=FALSE)

  # for compatability with other outputs
  parms <- s_res
  parms$index <- parms$estimate
  parms$parm <- log(parms$index/(1-parms$index))
  parms$estimate <- NULL
  
  results <- list(model=model,
       AIC=model$results$AICc, npar=model$results$npar,
       model.name=model.name, model.yrs=model.yrs,
       residency=r_res,
       survival=s_res,
       recapture=p_res,
       recap1=p1_res,
       parms=parms[ , c('years','parm','se','index','lcl','ucl','rank')],
       group=cesobj$group, 
       spp=cesobj$spp,
       spp.name=cesobj$spp.name)
  class(results) <- c('ces', 'markfit')
  
  return(results)
}

