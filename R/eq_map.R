#'@title Function eq_map
#'@description This function, eq_map() , takes an argument data containing
#'the filtered data frame with earthquakes to visualize.
#'The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each
#'point with in pop up window containing annotation data stored in a column of the data frame.
#'The user is be able to choose which column is used for the annotation in the pop-up with a
#'function argument named annot_col. Each earthquake is shown with a circle, and the radius of the
#'circle is proportional to the earthquake's magnitude (EQ_PRIMARY).
#'@param  eq - A dataframe of the data to map.
#'@param annot_col - The name of the field to use for the annotation
#'@examples
#'library(magrittr)
#'df<-readr::read_delim(system.file("extdata", "signif.txt", package="Earthquake"), delim = "\t")
#'df<- df %>% eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
#'  eq_map(df,annot_col = "DATE")
#'@export
eq_map<-function(eq,annot_col)
{
  if(class(eq[,annot_col])== "Date")
  {
    eq[,annot_col]<-as.character(eq[,annot_col])
  }
  leaflet::leaflet(data = eq) %>% leaflet::addTiles() %>% leaflet::addCircleMarkers(stroke = TRUE,weight=1,radius=eq$EQ_PRIMARY, popup = lapply(eq[,annot_col],htmltools::HTML))
}


#'@title Function eq_create_label
#'@description eq_create_label()  takes the dataset as an argument and creates an HTML label
#'that can be used as the annotation text in the leaflet map. This function puts together a
#'character string for each earthquake that will show the cleaned location (as cleaned by the eq_location_clean()
#'function created in Module 1), the magnitude (EQ_PRIMARY), and the
#'total number of deaths (TOTAL_DEATHS), with boldface labels for
#'each ("Location", "Total deaths", and "Magnitude"). If an earthquake is missing values for any of these,
#'both the label and the value should be skipped for that element of the tag.
#'@param  df - A dataframe of containing 3 columns LABELS,EQ_PRIMARY and DEATHS.
#'@examples
#'library(magrittr)
#'labels<-readr::read_delim(system.file("extdata", "signif.txt", package="Earthquake"), delim = "\t")
#'lables<-labels %>% eq_clean_data %>% eq_create_label
#'head(labels)
#'@export
eq_create_label<-function(df)
{
  location_name<-paste("<b>Location:</b>", df$LABELS, "<br />")
  richter<-paste("<b>Magnitude:</b>", df$EQ_PRIMARY, "<br />")
  deaths<-ifelse(is.na(df$DEATHS),"",paste("<b>Total Deaths:</b>", df$DEATHS, "<br />"))
  return(paste(location_name,richter,deaths))
}


