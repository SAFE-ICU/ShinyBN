check.discrete<-function(DiscreteData)
{
  if(sum(sapply(DiscreteData,is.numeric))>0)
  {
    shinyalert("Data has numeric variables, you can discretize the data using the available methods in the app ", type = "info")
  }
}
