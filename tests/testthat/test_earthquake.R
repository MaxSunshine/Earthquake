test_that("Dataframe is clean", {
  #eq_clean_data calls eq_location_clean so a pass of eq_clean_data is a pass for eq_location
  df<-readr::read_delim(system.file("extdata", "signif.txt", package="Earthquake"), delim = "\t")
  df <- df %>% eq_clean_data()
  expect_that( "DATE" %in% colnames(df),  is_true())
  expect_that( "LABELS" %in% colnames(df),  is_true())
  expect_that( "LONGITUDE" %in% colnames(df),  is_true())
  expect_that( "LATITUDE" %in% colnames(df),  is_true())
  expect_that( as.character(class(df$DATE))=="Date",  is_true())
  expect_that( as.character(class(df$LONGITUDE))=="numeric",  is_true())
  expect_that( as.character(class(df$LATITUDE))=="numeric",  is_true())
  expect_that(nrow(subset(df,grepl(df$LABELS, pattern = ":")))==0,is_true())

})
