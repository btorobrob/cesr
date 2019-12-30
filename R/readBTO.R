readBTO <- function (file, species=0) 
{
  if ( species==0 ){
    errtext=c('You must specify a Euring species code')
    stop(errtext)
  }
  result=as.data.frame(readLines(file))  ## this is MUCH faster than read.table
  
  # split the input lines
  result$ring=substr(result[,1],1,7)
  result$species=as.numeric(species)
  result$agec=substr(result[,1],15,16)
  result$sex=substr(result[,1],18,18)
  result$siteID=substr(result[,1],20,23)
  result$year=as.numeric(substr(result[,1],25,28))
  result$v1=substr(result[,1],30,30)
  result$v2=substr(result[,1],32,32)
  result$v3=substr(result[,1],34,34)
  result$v4=substr(result[,1],36,36)
  result$v5=substr(result[,1],38,38)
  result$v6=substr(result[,1],40,40)
  result$v7=substr(result[,1],42,42)
  result$v8=substr(result[,1],44,44)
  result$v9=substr(result[,1],46,46)
  result$v10=substr(result[,1],48,48)
  result$v11=substr(result[,1],50,50)
  result$v12=substr(result[,1],52,52)
  result[,1]=NULL
  
  result$age <- 0  # do this here since fewer rows
  result$age[result$agec=='JV'] <- 3
  result$age[result$agec=='AD'] <- 4
  result$sex <- as.factor(toupper(result$sex))
  result$sex[!result$sex %in% c("M", "F")] <- NA
  result$sex <- droplevels(result$sex)
  
  # unstack the data
  t1=result[result[7]=='1',c(1,2,4,5,6,19)]; t1$visit=1; 
  t2=result[result[8]=='1',c(1,2,4,5,6,19)]; t2$visit=2
  t3=result[result[9]=='1',c(1,2,4,5,6,19)]; t3$visit=3
  t4=result[result[10]=='1',c(1,2,4,5,6,19)]; t4$visit=4
  t5=result[result[11]=='1',c(1,2,4,5,6,19)]; t5$visit=5
  t6=result[result[12]=='1',c(1,2,4,5,6,19)]; t6$visit=6
  t7=result[result[13]=='1',c(1,2,4,5,6,19)]; t7$visit=7
  t8=result[result[14]=='1',c(1,2,4,5,6,19)]; t8$visit=8
  t9=result[result[15]=='1',c(1,2,4,5,6,19)]; t9$visit=9
  t10=result[result[16]=='1',c(1,2,4,5,6,19)]; t10$visit=10
  t11=result[result[17]=='1',c(1,2,4,5,6,19)]; t11$visit=11
  t12=result[result[18]=='1',c(1,2,4,5,6,19)]; t12$visit=12
  
  result <-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)  
  result$sitename <- as.character(as.factor(result$siteID))
  result$site <- as.numeric(as.factor(result$siteID))
  result$siteID <- NULL
  result$race <- result$species
  result$species <- 10 * floor(result$species/10)
  # Create extra variables to match Euring format
  result$countryID='GBT'
  result$day<-NA
  result$month<-NA
  result$lat <- NA 
  result$long <- NA 
  result$habitat <- NA
  result$NetLength <- NA
  
  class(result) <- c("ces", "data", "data.frame")
  attr(result, "country") = 'GBT'
  result
}