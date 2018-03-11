custom.association<-function(data,type)
{
  associationData = c()
  varNames = names(data)
  for(i in 1:(ncol(data)-1))
  {
    for(j in (i+1):ncol(data))
    {
      tryCatch({
        if(type == "cramer's V")
        {
          assocVal = CramerV(table(data[,c(i,j)]))
          associationData = rbind(associationData,c(varNames[i],varNames[j],assocVal))
        }
        else if(type == "Cohen's D")
        {
          assocVal = CohenD(table(data[,c(i,j)]))[1]
          associationData = rbind(associationData,c(varNames[i],varNames[j],assocVal))
        }
        else if(type =="Goodman Kruskal lambda")
        {
          assocVal = Lambda(table(data[,c(i,j)]))
          associationData = rbind(associationData,c(varNames[i],varNames[j],assocVal))
        }
        else if(type == "Tschuprow's T")
        {
          assocVal = TschuprowT(table(data[,c(i,j)]))
          associationData = rbind(associationData,c(varNames[i],varNames[j],assocVal))
        }

      },
      error = function(e)
      {
        assocVal = 0
        associationData = rbind(associationData,c(varNames[i],varNames[j],assocVal))
      })
    }
  }
  return(associationData)

}
