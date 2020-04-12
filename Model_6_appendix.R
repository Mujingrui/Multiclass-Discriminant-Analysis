#### Model6
library(MASS)
library(CVTuningCov)
p<-800
AR_6<-AR1(p, rho=0.8)

beta_61<-c(rep(0,800))
beta_62<-c(rep(1.2,8),rep(0,792))
beta_63<-c(rep(-1.2,4),rep(1.2,4),rep(0,792))
beta_64<-c(1.2,-1.2,1.2,-1.2,1.2,-1.2,1.2,-1.2,rep(0,792))

mu_61<-AR_6%*%beta_61
mu_62<-AR_6%*%beta_62
mu_63<-AR_6%*%beta_63
mu_64<-AR_6%*%beta_64

#data
dataX_61<-mvrnorm(n=75,mu=mu_61,Sigma=AR_6)
dataY_61<-matrix(1,nrow=75,ncol=1)
dataX_62<-mvrnorm(n=75,mu=mu_62,Sigma=AR_6)
dataY_62<-matrix(2,nrow=75,ncol=1)
dataX_63<-mvrnorm(n=75,mu=mu_63,Sigma=AR_6)
dataY_63<-matrix(3,nrow=75,ncol=1)
dataX_64<-mvrnorm(n=75,mu=mu_64,Sigma=AR_6)
dataY_64<-matrix(4,nrow=75,ncol=1)
dataX_6<-rbind(dataX_61,dataX_62,dataX_63,dataX_64)
dataY_6<-rbind(dataY_61,dataY_62,dataY_63,dataY_64)
data_6<-cbind(dataX_6,dataY_6)

##train data
Matrix_train6 <- matrix(0,nrow=210,ncol=50)
for(i in 1:50){
  Matrix_train6[,i]<-sample(nrow(data_6),0.7*nrow(data_6))
}
train_dataX6<-matrix(0,nrow=210,ncol=40000)
for(j in 1:50){
  train_dataX6[,(800*(j-1)+1):(800*j)]<-dataX_6[Matrix_train6[,j],]
}
train_dataY6<-matrix(0,nrow=210,ncol=50)
for(k in 1:50){
  train_dataY6[,k]<-dataY_6[Matrix_train6[,k]]
}

###test data
test_dataX6<-matrix(0,nrow=90,ncol=40000)
for(m in 1:50){
  test_dataX6[,(800*(m-1)+1):(800*m)]<-dataX_6[-(Matrix_train6[,m]),]
}
test_dataY6<-matrix(0,nrow=90,ncol=50)
for(l in 1:50){
  test_dataY6[,l]<-dataY_6[-(Matrix_train6[,l])]
}

##
library(msda)
lambda_min6<-rep(0,50)
id_min6<-rep(0,50)
pred6<-matrix(0,nrow=90,ncol=50)
nonzero_coef6<-rep(0,50)
for(i in 1:50){
  lambda_min6[i]<-cv.msda(x=train_dataX6[,(800*(i-1)+1):(800*i)],y=train_dataY6[,i],nfolds=5,lambda.opt ="max")$lambda.min
  id_min6[i]<-which(cv.msda(x=train_dataX6[,(800*(i-1)+1):(800*i)],y=train_dataY6[,i],nfolds=5,lambda.opt ="max")$lambda == lambda_min6[i])
  pred6[,i]<-predict(cv.msda(x=train_dataX6[,(800*(i-1)+1):(800*i)],y=train_dataY6[,i],nfolds=5,lambda.opt ="max")$msda.fit,test_dataX6[,(800*(i-1)+1):(800*i)])[,id_min6[i]]
  nonzero_coef6[i]<-cv.msda(x=train_dataX6[,(800*(i-1)+1):(800*i)],y=train_dataY6[,i],nfolds=5,lambda.opt ="max")$msda.fit$df[id_min6[i]]
}
count_error6<-rep(0,50)
for(j in 1:50){
  for(k in 1:90){
    if(pred6[k,j] != test_dataY6[k,j]){
      count_error6[j]<-count_error6[j]+1
    }
  }
}
count_error_rate6 <- count_error6/90
error_rate6 <- median(count_error_rate6)
sd_error6<-round(sd(count_error_rate6),4)
nonzero6 <- median(nonzero_coef6)
sd_nonzero6 <- round(sd(nonzero_coef6),4)

####PLDA
lambda_best<-rep(0,50)
best_id<-rep(0,50)
predict_plda<-matrix(0,nrow=90,ncol=50)
K_best<-rep(0,50)
nonzero_plda6<-matrix(0,nrow=50,ncol=3)

for(k in 1:50){
  lambda_best[k]<-PenalizedLDA.cv(train_dataX6[,(800*(k-1)+1):(800*k)],train_dataY6[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestlambda
  best_id[k]<-which(PenalizedLDA.cv(train_dataX6[,(800*(k-1)+1):(800*k)],train_dataY6[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$lambda==lambda_best[k])
  K_best[k]<-PenalizedLDA.cv(train_dataX6[,(800*(k-1)+1):(800*k)],train_dataY6[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestK
  predict_plda[,k]<-PenalizedLDA(train_dataX6[,(800*(k-1)+1):(800*k)],train_dataY6[,k],xte=test_dataX6[,(800*(k-1)+1):(800*k)],lambda=lambda_best[k],K=K_best[k])$ypred[,K_best[k]]
  nonzero_plda6[k,]<-PenalizedLDA.cv(train_dataX6[,(800*(k-1)+1):(800*k)],train_dataY6[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$nnonzero[best_id[k],]
  
}
plda_error<-rep(0,50)
for(j in 1:50){
  for(k in 1:90){
    if(predict_plda[k,j] != test_dataY5[k,j]){
      plda_error[j]<-plda_error[j]+1
    }
  }
}
plda_error6<-plda_error/90
plda_error_rate6 <-summary(plda_error6)[3]
sd_error_rate6 <- round(sd(plda_error6),4)
