

eq_map<-function(eq,annot_col)
{

   leaflet::leaflet(data = eq) %>% leaflet::addTiles() %>% leaflet::addCircleMarkers(stroke = TRUE,weight=1, popup = lapply(eq[,annot_col],htmltools::HTML))
}

#ldf %>% eq_map(annot_col="DATE")


eq_create_label<-function(df)
{
  location_name<-paste("<b>Location:</b>", df$LABELS, "<br />")
  richter<-paste("<b>Magnitude:</b>", df$EQ_PRIMARY, "<br />")
  deaths<-ifelse(is.na(df$DEATHS),"",paste("<b>Total Deaths:</b>", df$DEATHS, "<br />"))
  return(paste(location_name,richter,deaths))
}


