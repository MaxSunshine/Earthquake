
extract_labels<-function(country,location="")
{
  if (is.na(location) | nchar(as.character(location))==0)
  {
    return(as.factor(lettercase::str_title_case(as.character(country))))
  }
  ldf<-as.data.frame(unlist(strsplit(as.character(location),"\\;")))
  colnames(ldf)<-c("labels")

  country<-gsub("\\(", "REPLACE_",country)
  country<-gsub("\\)", "_REPLACE",country)
  ldf$labels<-gsub("\\(", "REPLACE_",ldf$labels)
  ldf$labels<-gsub("\\)", "_REPLACE",ldf$labels)


  ldf$labels<-as.character(ldf$labels)
  country2<-as.character(country)
  country<-paste(country,":",sep="")

  d<-ldf %>% dplyr::filter(str_detect(labels, country)) %>% unique
  d2<-ldf %>% dplyr::filter(str_detect(labels, country2)) %>% unique

  #return("x")
  if(nrow(d)==0 )
  {
    country2<-paste(country2,";",sep="")
    return(as.factor(lettercase::str_title_case(paste(trimws(gsub(country2,"",location)), collapse=", "))))

  }
  else
  {
    retval<-paste(trimws(gsub(country,"",d$labels)), collapse=", ")
    retval<-gsub( "REPLACE_","\\(",retval)
    retval<-gsub( "_REPLACE","\\)",retval)

    return(as.factor(lettercase::str_title_case(retval)))
  }

}


quakeDate<-function (YEAR,MONTH=1,DAY=1)
{
  if(is.na(MONTH))
    MONTH<-1
  if(is.na(DAY))
    DAY<-1
  if (YEAR>0)
  {
    yr<-as.Date(paste(YEAR,MONTH,DAY,sep="-"))
  }
  else
  {

    d1<-as.integer(diff(c(as.Date("0000-01-01",format="%Y-%m-%d"),as.Date(paste(-YEAR,"01-01",sep="-"),format="%Y-%m-%d") )) )

    yr<-as.Date("0000-01-01",format="%Y-%m-%d")-d1
    # print(paste(yr,d1))
    if (!(MONTH==1 & DAY==1))
    {
      yr<-DescTools::AddMonths(yr,MONTH-1)+DAY-1
      #print(paste(yr, "RIGHT"))
    }
    else
      yr<-yr+1

  }
  return(as.Date(yr,format="%Y-%m-%d"))
}

#'@title Function eq_location_clean
#'@description This is the eq_location_clean. This function cleans the LOCATION_NAME column by stripping out
#'the country name (including the colon) and converts names to title case (as opposed to all caps).
#'The function creates a cleaned up version ot the LOCATION_NAME called LABELS.
#'@export
#'
eq_location_clean<-function(df)
{
  #first pass
  df$LABELS<-mapply(extract_labels,tolower(df$COUNTRY),tolower(df$LOCATION_NAME))
  #second pass
  df$LABELS<-gsub(".*:","",df$LABELS)
}

#'@title Function eq_clean_data
#'@description This is the eq_clean_data. The function eq_clean_data takes
#'raw NOAA data frame and returns a clean data frame.The clean data frame has the following
#'1. A date column created by uniting the year, month, day and converted to a Date class
#'2. LATITUDE and LONGITUDE columns converted to numeric class
#'
#'@export
eq_clean_data<-function(y)
{
  #x<-get(deparse(substitute(y)))
  x<-transform(as.data.frame(y),
               LONGITUDE=as.numeric(LONGITUDE),
               LATITUDE=as.numeric(LATITUDE),
               EQ_PRIMARY=as.numeric(EQ_PRIMARY),
               DEATHS=as.numeric(DEATHS),
               COUNTRY= as.factor(COUNTRY))
  x$DATE<-mapply(quakeDate,x$YEAR,x$MONTH,x$DAY)%>% as.Date(origin='1970-01-01')
  #first pass
  x$LABELS<-mapply(extract_labels,tolower(x$COUNTRY),tolower(x$LOCATION_NAME))
  #second pass``
  x$LABELS<-gsub(".*:","",x$LABELS)
  return(x)
}

