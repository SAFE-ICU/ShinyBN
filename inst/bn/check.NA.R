check.NA<-function(DiscreteData)
{
  if(sum(is.na(DiscreteData))>0)
  {
    shinyalert(c("Data has missing values, you can impute the data using the app "), type = "info")
  }
}
