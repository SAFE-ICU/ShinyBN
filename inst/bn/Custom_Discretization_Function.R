


load("C:/Users/tavsethi/Documents/R/win-library/3.4/ShinyBN/bn/newdata.RData")


sum(apply(a,2,is.numeric))
lapply(a,class)

(which(lapply(a,class)=="numeric") %in% TRUE)
quantile(a$cz_pop2000)




bnlearn::discretize(a,method = "quantile")
column_class <- lapply(a,class)

ind_int <- as.vector(which(column_class %in% "integer"))
ind_num <- as.vector(which(column_class %in% "numeric"))

a[ind_int] <- lapply(a[ind_int],as.numeric)


b <- a
b$intersects_msa <- NULL

quantile(b$tax_st_diff_top20)

bnlearn::discretize



##############  Custom Discretization Code  ###################
Custom_Discretize <- function(x){

  out <- try(arules::discretize(as.numeric(x),method="cluster"),silent = TRUE)
  if(class(out)=="try-error"){
    out <- try(bnlearn::discretize(data.frame(x),method="quantile"),silent = TRUE)
    if(class(out)=="try-error"){
      out <- try(bnlearn::discretize(data.frame(x),method="interval"),silent = TRUE)
      if(class(out)=="try-error"){
        out <- data.frame(x)
        names(out) <- names(x)
      }

    }

  }
  return(out)
}

########################### Data Discretization ##################

b <- data.frame(apply(a,2,Custom_Discretize))
colnames(b) <- colnames(a)

save(b,file="Corrected_Discretization_Income_Inequality.RData")
