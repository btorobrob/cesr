summary.ces.markfit <-
function(x){
    
  if ( !class(x)[1] == 'ces' | !class(x)[2] == 'markfit' )
    stop("No information to summarise\n")
  
  if( is.na(x$group$name) ){
    cat(sprintf("%s %5.3f %s %5.3f %s", 'Probability of residency:', x$residency$estimate, '\u00B1', x$residency$se, '\n'))
  } else {
    for ( i in 1:length(x$group$levels) )
      cat(sprintf("%s %s %s %5.3f %s %5.3f %s", 'Group', x$group$levels[i], ': Probability of residency:', x$residency$estimate[i], '\u00B1', x$residency$se[i], '\n'))
  }

  if( x$model.name == 'trend' ) {
    if( is.na(x$group$name) ){
      sloperow <- grep('Tind', rownames(x$model$results$beta))
      slope <- round(x$model$results$beta[sloperow, 1], 3)
      se <- round(x$model$results$beta[sloperow, 2], 3)
      p <- round(pt(slope/se, df=x$model.yrs-2, lower.tail=FALSE), 3)
      cat(paste('Trend in (logit) survival: ', slope, ' \u00B1 ', se, '; t=', round(slope/se,2), ', P=', p, '\n', sep=''))
      # now convert into probabilities
      end.survival <- x$survival[nrow(x$survival), 'estimate']
      begin.survival <- x$survival[(nrow(x$survival)-x$model.yrs+1), 'estimate']
      total.change <- end.survival/begin.survival
      ann.change <- total.change ^ (1/x$model.yrs)
      if( end.survival > begin.survival )
        cat(paste0('(This is equivalent to a ', round(100*(total.change-1),1), '% increase over ', x$model.yrs,
                   ' years, at an average of ', round(100*(ann.change-1),1), '% per year)\n'))
      else if( begin.survival > end.survival )
        cat(paste0('(This is equivalent to a ', round(100*(1-total.change),1), '% decrease over ', x$model.yrs,
                   ' years, at an average of ', round(100*(1-ann.change),1), '% per year)\n'))
    } else {
      # need to do this
    }
  }  
    
  if( x$model.name == 'constant' ) {
    if( is.na(x$group$name) ){
      rown <- nrow(x$survival)
      if( x$model.yrs == rown ) # constant survival
        cat(paste('Survival over', x$model.yrs, 'years is:', round(x$survival[rown, 2],3), '\u00B1', round(x$survival[rown, 3],3), '\n')) 
      else { # comparison period
        tmp <- x$survival[1:(rown-x$model.yrs), ]
        minphi <- round(min(tmp[ , 2]), 3)
        maxphi <- round(max(tmp[ , 2]), 3)
        cat(paste('Survival in last', x$model.yrs, 'years is:', round(x$survival[rown, 2],3), '\u00B1', round(x$survival[rown, 3],3),
                  'Previously it varied between', minphi, 'and', maxphi, '\n')) 
      }      
    } else {
      # need to do ths
    }
  } else if ( x$model.name == 'compare' ) {
    if( is.na(x$group$name) ){
      rown <- nrow(x$survival)
      tmp <- x$survival[1:(rown-x$model.yrs), ]
      cat(paste0('Survival in last ', x$model.yrs, ' years was: ', round(x$survival[rown-1, 2],3), ' \u00B1 ', round(x$survival[rown-1, 3],3),
                ', last year it was: ', round(x$survival[rown, 2],3), ' \u00B1 ', round(x$survival[rown, 3],3), '.'))
      if( rown > (x$model.yrs+1) ){ # check that there are some years before the compare period!    
        minphi <- round(min(tmp[ , 2]), 3)
        maxphi <- round(max(tmp[ , 2]), 3)
        cat(paste0(' Previously it varied between ', minphi, ' and ', maxphi, '.\n')) 
      } else {
        cat('\n')
      }
    } else {
      # need to do this
    }
  } else {
    if( is.na(x$group$name) ){
      mean_s <- mean(x$survival[,2])
      cat(sprintf("%s %5.3f %s %5.3f %s %5.3f %s",'Annual survival varies between', min(x$survival[,2]), 
                  'and', max(x$survival[,2]), 'with an average of', mean_s,'\n'))
    } else {
      for ( i in 1:length(x$group$levels) ){
        tmp <- x$survival[x$survival$group==x$group$levels[i], ]
        mean_s <- mean(tmp[,3])
        cat(sprintf("%s %s %s %5.3f %s %5.3f %s %5.3f %s",'Group', x$group$levels[i], ': Annual survival varies between', min(tmp[,3]), 
                    'and', max(tmp[,3]), 'with an average of', mean_s,'\n'))
      }
    }
  }
  check.parms <- sum(abs(x$parms$parm) > 5) # these are almost certainly rubbish!
  if( check.parms > 0 ){
    wmsg <- paste(check.parms, "estimates of survival are suspect, check the results carefully!")
    warning(wmsg, call.=FALSE, immediate.=FALSE)
  }
      
  cat(sprintf("%s %5.3f %s %5.3f %s %d %s",'Recapture probabilities vary between', 
              min(x$recapture[,2]), 'and', max(x$recapture[,2]),'across ',
              length(x$recapture[,2]), 'sites\n'))
}