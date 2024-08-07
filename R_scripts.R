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
  residual_value_agri<-data_trial$log_labor_share_agri_manu-(1-theta[1])*data_trial$log_Pa+(1-theta[1])*data_trial$log_Pm-
    (1-theta[1])*(theta[2]-1)*data_trial$log_expenditure_pc_pwt-(theta[2]-1)*data_trial$log_va_share_man-
    x%*%y1
  residual_value_serv_exclgs<-data_trial$log_labor_share_serv_manu_exclgs-(1-theta[1])*data_trial$log_Ps+(1-theta[1])*data_trial$log_Pm-
    (1-theta[1])*(theta[3]-1)*data_trial$log_expenditure_pc_pwt-(theta[3]-1)*data_trial$log_va_share_man-
    x%*%y2
  residual_v<-cbind(residual_value_agri,residual_value_serv_exclgs)
}

#Trial on the function
theta<-c(1:81)
trail<-residual_value(theta,data2)
trail<-as.data.frame(trail)

moments <-function(theta, data_trial){
  #Generate a moment condition matrix
  mat<-matrix(0, nrow = num_obser, ncol = num_moment)
  mat<-as.data.frame(mat)
  mat[,1]<-data_trial$log_expenditure_pc_pwt
  mat[,2]<-data_trial$log_Pa
  mat[,3]<-data_trial$log_Pm
  mat[,4]<-data_trial$log_va_share_man
  for (i in 5:43){
    mat[,i]<-data_trial[,i+3]
  }
  mat[,44]<-1
  mat[,45]<-data_trial$log_expenditure_pc_pwt
  mat[,46]<-data_trial$log_Pa
  mat[,47]<-data_trial$log_Pm
  mat[,48]<-data_trial$log_va_share_man
  for(i in 49:87){
    mat[,i]<-data_trial[,i-41]
  }
  mat[,88]<-1
  residual_val<-residual_value(theta,data_trial)
  residual_val<-as.data.frame(residual_val)
  for (i in 1:44){
    mat[,i]<-mat[,i]*residual_val[,1]
    mat[,44+i]<-mat[,44+i]*residual_val[,2]
  }
  mat<-as.matrix(mat)
  return(mat)
}
#Trial on this moments generating function.
moment_trial<-moments(theta,data2)

#Gmm estimation
#First get an initial guess of parameters
init<-c(0.5,0.25,1.25)
for (i in 1:78){
  init<-append(init,1)
}

data_new<-data.matrix(data2)
my_gmm <- gmm(moments, x = data2, t0 = init, type = "twoStep", crit = 1e-25, wmatrix = "optimal", method = "Nelder-Mead", control = list(reltol = 1e-25, maxit = 20000))

summary(my_gmm)
