#This is a simple file on how to add dummy variables with 
install.packages("foreign")
require("gmm")

library(dplyr)
library(foreign)

data<-cross_country_data

#Generate country list
country_list=c()
iso<-data$country_iso
for (i in 1:dim(data)[1]){
  jug<-iso[i] %in% country_list
  if(jug==0){
    country_list <- append(country_list,iso[i])
  }
}

#Creating dummies and assign them names
numrow <-dim(data)[1]

number_countries <- 39
for(i in 1:number_countries) {                                   # Head of for-loop
  new <- ifelse(data$country_no== i, 1, 0)                       # Create new column identified as dummy
  data[ , ncol(data) + 1] <- new                                 # Append this column to the existing data
  colnames(data)[ncol(data)] <- country_list[i]                  # Rename column name according to the existing list
}


#Out of discretion, we clean the data by choosing the subset with values not equal to NA
data1 <- data[!(is.na(data$log_expenditure_pc_pwt)|is.na(data$log_netx_agri)|is.na(data$log_netx_manu)) , ]

#First we name a few of necessary variables
ln_exp <- data1$log_expenditure_pc_pwt

#Add observations where there are observations
#country_list_a=c()
#country_list_s=c()
#for (i in 1:number_countries) {
#  sum1 = 0
#  sum1 <- mean(data1[ , ncol(cross_country_data) + i])
#  if(sum1>0){
#    country_list_a <- append(country_list_a,country_list[i])
#    country_list_s <- append(country_list_s,country_list[i])
#  }
#}
#sum1 <- mean(data1[ , 86 + 1])
#Let's not consider bootstrap first
#This is a non linear model and therefore could not be estimated with ols or 2sls regression.
#We will use GMM to estimate the results.

#To make things easier, let's first generate a new data framework where only variales in interest enters as columns.
variable_interest<-c('log_labor_share_agri_manu', 'log_expenditure_pc_pwt','log_Pa','log_Pm', 'log_va_share_man',
  'log_labor_share_serv_manu_exclgs','log_Ps')
regressors<-c(variable_interest,country_list)
data2<-subset(data1, select=regressors)
#First we write functions to generate moment conditions.
#In our specification here, theta is a vector of parameters in the following order: \sigma, \epsilon_a, \epsilon_s
#The return here should be a n\times2q matrix, where q is the number of regressors
num_obser <-nrow(data2)
num_moment <- 88 #number of moment conditions
num_para <- 81 #number of parameters

#Functions that gives residuals for each observation
residual_value<-function(theta,data_trial){
  x<-data.matrix(data_trial[,8:46])
  y1<-as.matrix(theta[4:42])
  y2<-as.matrix(theta[43:81])
  residual_value_agri<-data_trial[,1]-(1-theta[1])*data_trial[,3]+(1-theta[1])*data_trial[,4]-
    (1-theta[1])*(theta[2]-1)*data_trial[,2]-(theta[2]-1)*data_trial[,5]-
    x%*%y1
  residual_value_serv_exclgs<-data_trial[6]-(1-theta[1])*data_trial[,7]+(1-theta[1])*data_trial[,4]-
    (1-theta[1])*(theta[3]-1)*data_trial[,2]-(theta[3]-1)*data_trial[,5]-
    x%*%y2
  residual_v<-cbind(residual_value_agri,residual_value_serv_exclgs)
}

#Trial on the function
theta<-c(1:81)
trail<-residual_value(theta,data_new)
trail<-as.data.frame(trail)

moments <-function(theta, data_trial){
  #Generate a moment condition matrix
  mat<-matrix(0, nrow = num_obser, ncol = num_moment)
  mat[,1]<-data_trial[,2]
  mat[,2]<-data_trial[,3]
  mat[,3]<-data_trial[,4]
  mat[,4]<-data_trial[,5]
  for (i in 5:43){
    mat[,i]<-data_trial[,i+3]
  }
  mat[,44]<-1
  mat[,45]<-data_trial[,2]
  mat[,46]<-data_trial[,7]
  mat[,47]<-data_trial[,4]
  mat[,48]<-data_trial[,5]
  for(i in 49:87){
    mat[,i]<-data_trial[,i-41]
  }
  mat[,88]<-1
  mat1<-data.matrix(mat[,1:44])
  mat2<-data.matrix(mat[,45:88])
  residual_val<-residual_value(theta,data_trial)
  y1 <- as.numeric(residual_val[, 1])
  y2 <- as.numeric(residual_val[,2])
  m1 <- mat1*as.vector(y1)
  m11 <- cbind(m1)
  m2 <- mat2*as.vector(y2)
  m22 <-cbind(m2)
  mat_final<-cbind(m11,m22)
  mat_final<-data.matrix(mat_final)
  return(mat_final)
}
#Trial on this moments generating function.
moment_trial<-moments(theta,data_new)
moment_trial<-data.matrix(moment_trial)
a<-moment_trial[1,1]
b<-data_new[,1]
b1<-as.numeric(b)


#Gmm estimation
#First get an initial guess of parameters
init<-c(0.5,0.25,1.25)
for (i in 1:78){
  init<-append(init,1)
}

data_new<-data.matrix(data2)
my_gmm <- gmm(moments, x = data2, t0 = init, type = "twoStep", wmatrix = "ident", control = list(reltol = 1e-25, maxit = 20000))

summary(my_gmm)
