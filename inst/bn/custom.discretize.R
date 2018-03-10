custom.discretize <- function(x,type)
{

  if(type == "quantile")
  {
    out <- try(bnlearn::discretize(data.frame(x),method="quantile"))
    if(class(out)=="try-error"){
      out <- try(bnlearn::discretize(data.frame(x),method="interval"))
      #print("interval")
      shinyalert("Failed to discretize all variables using the desired method, used interval discretization for them instead",type = "info")
      if(class(out)=="try-error"){
        shinyalert("Failed to discretize some variables in the data. Please try again using some other method or input a discretized data",type = "error")
      }

    }
    return(out)
  }
  else if(type == "hybrid")
  {
    out <- try(arules::discretize(as.numeric(x),method="cluster"))
    if(class(out)=="try-error"){
      out <- try(arules::discretize(as.numeric(x),method="frequency"))
      if(class(out)=="try-error"){
        out <- try(bnlearn::discretize(data.frame(x),method="quantile"))
        if(class(out)=="try-error"){
          out <- try(bnlearn::discretize(data.frame(x),method="interval"))
          #print("interval")
          if(class(out)=="try-error")
          {
            shinyalert("Failed to discretize some variables in the data. Please try again using some other method or input a discretized data",type = "error")
          }
        }

      }

    }
    return(out)

  }
  else
  {
    out <- try(arules::discretize(x,method=type))
    print(out)
    if(class(out)=="try-error"){
      out <- try(bnlearn::discretize(data.frame(x),method="interval"))
      #print("interval")
      shinyalert("Failed to discretize all variables using the desired method, used interval discretization for them instead",type = "info")
      if(class(out)=="try-error"){
        shinyalert("Failed to discretize some variables in the data. Please try again using some other method or input a discretized data",type = "error")
      }
    }
    return(out)
  }

}
