setceslang <-
function(lang){    

  if( !mode(lang) == "character" )
    stop("Please supply a (quoted) character string\n")
  if( lang == "" )
    stop("Options are Latin, Danish, Dutch, English, Finnish, French, German, Italian, Norwegian, Polish, Portuguese, Spanish, Swedish\n") 
  if( lang %in% c('Latin', 'English', 'Danish', 'Dutch', 'Finnish', 'French', 'German',
                  'Italian', 'Norwegian', 'Polish', 'Portuguese', 'Spanish', 'Swedish') ) {
    options(ceslang=lang)
  } else {
    cat(lang, "is not recognised, using default 'Latin' instead\n")
    cat("Other options: Danish, Dutch, English, Finnish, French, German, Italian, Norwegian, Polish, Portuguese, Spanish or Swedish\n") 
    options(ceslang='Latin')
  }
}

