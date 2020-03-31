

#Function to coerce fixest object into stargazer table
fixest_to_coeftest <- function(fixest.object,ds){
  require(fixest)
  #Check if arg1 is fixest class
  if(!class(fixest.object)=="fixest"){
    stop("Not a fixest object")
  }
  
  #Extract the coefficient table
  df <- fixest.object$coeftable 
  
  if(length(fixest.object$obsRemoved)>0){
    cluster.var <- ds$tribe[-fixest.object$obsRemoved]
  } else {
    cluster.var <- ds$tribe
  }
  
  
  df.mod <- df %>%
    mutate(se = as.numeric(sqrt(diag(vcov(fixest.object,cluster=cluster.var)))),
           stat = Estimate/se,
           p = 2*pt(-abs(stat),df=nobs(fixest.object)-fixest.object$nparams)) %>%
    dplyr::select(Estimate,se,stat,p)
  
  
  #Coerce dataframe into matrix
  coeftest.object <- as.matrix(df.mod)
  
  #Capture variable names from df and rename columns to conform to coeftest
  new_names <- list(rownames(df),
                    c("Estimate","Std. Error","t value","Pr(>|t|)"))
  
  #Assign names
  dimnames(coeftest.object) <- new_names
  
  #re-class to coeftest
  class(coeftest.object) <- 'coeftest'
  
  return(coeftest.object)
  
}

#Function to coerce lme object into stargazer table
lme_to_coeftest <- function(lme.object){
  require(clubSandwich)
  #Check if arg1 is fixest class
  if(!class(lme.object)=="lme"){
    stop("Not an lme object")
  }
  
  #Extract the coefficient table
  df <- coef_test(lme.object,
                  vcov = vcovCR(lme.object,
                                type="CR2"))
  
  #Coerce dataframe into matrix
  coeftest.object <- as.matrix(df[,c(1,2,3,5)])
  
  #Capture variable names from df and rename columns to conform to coeftest
  new_names <- list(rownames(df),
                    c("Estimate","Std. Error","t value","Pr(>|t|)"))
  
  #Assign names
  dimnames(coeftest.object) <- new_names
  
  #re-class to coeftest
  class(coeftest.object) <- 'coeftest'
  
  return(coeftest.object)
  
}

gamlss_clustered_vcov <- function(gamlss.object,cluster=NA){
  require(gamlss)
  # if(is.na(cluster)){
  #   #set cluster to be index
  #   #fc <- index(gamlss.object$mu.x)
  # } else {
  #   #check length conforms
  #   if(!length(cluster)==length(gamlss.object$y)){
  #     stop("Clustering vector provided does not conform; check if there are NAs dropped in estimation.")
  #   }
  # }
  esample <- as.numeric(rownames(as.matrix(gamlss.object$mu.qr$qr)))
  fc <- cluster  #isolates the groups used in estimation
  
  # Calculates the new Meat portion of our covariance matrix
  m <- length(unique(fc))
  k <- length(gamlss.object$mu.coefficients)
  u <- gamlss.object$mu.qr$qr
  u.clust <- matrix(NA, nrow=m, ncol=k)
  for(j in 1:k){
    u.clust[,j] <- tapply(u[,j], as.numeric(fc), sum)
  }
  u.clust <- u.clust[!rowSums(u.clust,dims = 1)==0,] #dropping all clusters with all zeros
  cl.vcov <- vcov(gamlss.object)[1:k,1:k]%*%( t(u.clust) %*% (u.clust))%*%vcov(gamlss.object)[1:k,1:k]
  return(cl.vcov)
}



ols_clustered <- function(dep.var,df,weighted){
  if(weighted){
    m.temp <- feols(formula(str_c(dep.var, " ~ time")),
                    data = df,
                    weights = ~ wgt) 
  } else {
    m.temp <- feols(formula(str_c(dep.var, " ~ time")),
                    data = df) 
  }
  
  model.out <- fixest_to_coeftest(m.temp,df)
  
  obs <- nobs(m.temp)
  
  
  return(list(results=model.out,obs=obs))
}

feols_clustered <- function(dep.var,df,weighted){
  if(weighted){
    m.temp <- feols(formula(str_c(dep.var, " ~ time | tribe")),
                    data = df,
                    weights = ~ wgt) 
  } else {
    m.temp <- feols(formula(str_c(dep.var, " ~ time | tribe")),
                    data = df) 
  }
  
  model.out <- fixest_to_coeftest(m.temp,df)
  
  obs <- nobs(m.temp)
  
  
  return(list(results=model.out,obs=obs))
}

glm_clustered <- function(dep.var,df,family){

  m.temp <- feglm(formula(str_c(dep.var, " ~ time")),
                  family = family,
                  data = df) 
  
  model.out <- fixest_to_coeftest(m.temp,df)
  
  obs <- nobs(m.temp)
  
  return(list(results=model.out,obs=obs))
}


sg_table <- function(model.shell,dep.name,...){
  require(stargazer)
  require(stringr)
  
  stargazer(map(model.shell,"results"),type="latex",
            ci=T,
            intercept.top = T,
            intercept.bottom = F,
            dep.var.caption = str_c("Dependent Variable: ",dep.name),
            #column.labels = col.names,
            model.numbers = T,
            covariate.labels = c("Historical (Intercept)","Present-day Change"),
            #title = "Extent of Area Occupied",
            add.lines = list(c("Obs",comma(map_dbl(model.shell,"obs")))),
            float.env = "sidewaystable",
            notes = c("95\\% confidence intervals in parentheses based on tribe-clustered standard errors."),
            notes.align = "l",
            header = F,
            ...)
}
