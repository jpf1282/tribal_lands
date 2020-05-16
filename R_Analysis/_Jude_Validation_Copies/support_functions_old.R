dep.var = "drt_median"
lme.object <- lme(formula(str_c(dep.var," ~ time")),
                  data = data_t1and2_long %>% drop_na(one_of(dep.var), time, tribe),
                  random = ~1|tribe)
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

stargazer::stargazer(lme_to_coeftest(lme.object),type="text")

fixest.object <- lm_model

fixest_to_coeftest <- function(fixest.object){
  require(fixest)
  #Check if arg1 is fixest class
  if(!class(fixest.object)=="fixest"){
    stop("Not a fixest object")
  }
  
  #Extract the coefficient table
  df <- fixest.object$coeftable 
  
  df.mod <- df %>%
    mutate(se = as.numeric(sqrt(diag(vcov(fixest.object,cluster=data_t1and2_long$tribe[-fixest.object$obsRemoved])))),
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


stargazer::stargazer(list(fixest_to_coeftest(lm_model),lme_to_coeftest(lme_model)),
                          type="text")


#There is some problem with the formula call in lme not accessing the variable dep.var


#Heat
dep.var = "h_100_hist"
stargazer(model.run.t1andt2(dep.var = "h_100_hist"),type="text",ci=TRUE)

#Drought
dep.var = "drt_median"
stargazer(model.run.t1andt2(dep.var = "drt_median"),type="text",ci=TRUE)



model.matrix(model.shell$lmm)




# Trim the > 1 value to .99999 for the model to fit
bezi_data <- mutate(data_t1and2_long, 
                    p_all = ifelse(p_all >= 1, .99999, p_all)) %>%
  filter(!is.na(p_all)) %>%
  dplyr::select(time, tribe, p_all,wgt)

bezi_model <- gamlss(p_all ~ time, family = BEZI, data = bezi_data, trace = F)

bezi_model_w <-  gamlss(p_all ~ time, family = BEZI, data = bezi_data, trace = F,weights = wgt)

tab_model(bezi_model,bezi_model_w, 
          show.zeroinf = F,
          show.se = T,
          show.intercept = F,
          vcov.fun = "gamlss_clustered_vcov",
          vcov.args = list(cluster = bezi_data$tribe))

summary(bezi_model)
summary(bezi_model_w)


#lmtest::coeftest(bezi_model,vcov = sandwich::vcovCL(bezi_model,cluster=bezi_data$tribe))
sqrt(diag(gamlss_clustered_vcov(bezi_model,cluster = bezi_data$tribe)))




################################################################
#Copied out of old version


#Function to coerce fixest object into stargazer table
fixest_to_coeftest <- function(fixest.object,df){
  require(fixest)
  #Check if arg1 is fixest class
  if(!class(fixest.object)=="fixest"){
    stop("Not a fixest object")
  }
  
  #Extract the coefficient table
  df <- fixest.object$coeftable 
  
  if(length(fixest.object$obsRemoved)>0){
    cluster.var <- df$tribe[-fixest.object$obsRemoved]
  } else {
    cluster.var <- df$tribe
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

###############################################
#Function that runs all specifications, clusters se and returns coeftest objects for printing with stargazer
model.run.t1andt2 <- function(dep.var){
  
  isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
    abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
  }
  
  #Considered a version where outliers are dropped - the fixest_to_coeftest function is too fragile (it currently calls data_t1andt2_long from outside of the function)
  model_data <- data_t1and2_long #%>%
  #filter(isnt_out_z((!!as.symbol(dep.var))))
  
  # Fixest models
  model.shell <- list()
  model.shell$lm <- feols(formula(str_c(dep.var," ~ time")),
                          data = model_data)
  model.shell$lmw <- feols(formula(str_c(dep.var," ~ time")),
                           data = model_data, weights = ~wgt)
  model.shell$fem <- feols(formula(str_c(dep.var," ~ time|tribe")),
                           data = model_data)
  model.shell$femw <- feols(formula(str_c(dep.var," ~ time|tribe")),
                            data = model_data, weights = ~wgt)
  
  # Linear Mixed Model
  model.shell$lmm <- lme(formula(str_c(dep.var," ~ time")),
                         data = model_data %>% drop_na(one_of(dep.var),time,tribe),
                         random = ~1|tribe)
  
  model.shell$lmmw <- lme(formula(str_c(dep.var," ~ time")),
                          data = model_data %>% drop_na(one_of(dep.var),time,tribe),
                          random = ~1|tribe,
                          weights = ~wgt)
  
  model.shell <- modify_at(model.shell,c("lm","lmw","fem","femw"),fixest_to_coeftest)
  model.shell <- modify_at(model.shell,c("lmm","lmmw"),lme_to_coeftest)
  
  return(model.shell)
}

#Robust standard errors following https://rpubs.com/cuborican/xtpoisson
#The function takes a gamlss object (output by gamlss) and a vector of conforming clustering variable
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
