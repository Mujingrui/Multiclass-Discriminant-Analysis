##### Model5
library(MASS)
# aaa <- matrix(c(10,3,3,2),2,2)
# VVV<-mvrnorm(n=75, rep(0, 2),aaa)
library(CVTuningCov)
p<-800
AR_5<-AR1(p, rho=0.5)
#beta
beta_51<-c(rep(0,800))
beta_52<-c(rep(1.2,8),rep(0,792))
beta_53<-c(rep(-1.2,4),rep(1.2,4),rep(0,792))
beta_54<-c(1.2,-1.2,1.2,-1.2,1.2,-1.2,1.2,-1.2,rep(0,792))
#mu
mu_51<-AR_5%*%beta_51
mu_52<-AR_5%*%beta_52
mu_53<-AR_5%*%beta_53
mu_54<-AR_5%*%beta_54
#data
dataX_51<-mvrnorm(n=75,mu=mu_51,Sigma=AR_5)
dataY_51<-matrix(1,nrow=75,ncol=1)
dataX_52<-mvrnorm(n=75,mu=mu_52,Sigma=AR_5)
dataY_52<-matrix(2,nrow=75,ncol=1)
dataX_53<-mvrnorm(n=75,mu=mu_53,Sigma=AR_5)
dataY_53<-matrix(3,nrow=75,ncol=1)
dataX_54<-mvrnorm(n=75,mu=mu_54,Sigma=AR_5)
dataY_54<-matrix(4,nrow=75,ncol=1)

# mean(data_31[,1])
dataX_5<-rbind(dataX_51,dataX_52,dataX_53,dataX_54)
dataY_5<-rbind(dataY_51,dataY_52,dataY_53,dataY_54)
data_5<-cbind(dataX_5,dataY_5)
######train
Matrix_train5<-matrix(0,nrow=210,ncol=50)
for(i in 1:50){
  Matrix_train5[,i]<-sample(nrow(data_5),0.7*nrow(data_5))
}
train_dataX5<-matrix(0,nrow=210,ncol=40000)
for(j in 1:50){
  train_dataX5[,(800*(j-1)+1):(800*j)]<-dataX_5[Matrix_train5[,j],]
}
# train_dataX[,1:127]<-X_GDS1615[Matrix_train[1],]
train_dataY5<-matrix(0,nrow=210,ncol=50)
for(k in 1:50){
  train_dataY5[,k]<-dataY_5[Matrix_train5[,k]]
}
class(train_dataY)
#####test
test_dataX5<-matrix(0,nrow=90,ncol=40000)
for(m in 1:50){
  test_dataX5[,(800*(m-1)+1):(800*m)]<-dataX_5[-(Matrix_train5[,m]),]
}
test_dataY5<-matrix(0,nrow=90,ncol=50)
for(l in 1:50){
  test_dataY5[,l]<-dataY_5[-(Matrix_train5[,l])]
}
##fit the data
# listfit<-vector("list",100)
lambda_min5<-rep(0,50)
id_min5<-rep(0,50)
predd5<-matrix(0,nrow=90,ncol=50)
nonzero_coef5<-rep(0,50)
for(i in 1:50){
  # listfit[i]<-cv.msda(x=train_dataX[,(127*(i-1)+1):(127*i)],y=train_dataY[,i],nfolds=5,lambda.opt ="max")$msda.fit
  lambda_min5[i]<-cv.msda(x=train_dataX5[,(800*(i-1)+1):(800*i)],y=train_dataY5[,i],nfolds=5,lambda.opt ="max")$lambda.min
  id_min5[i]<-which(cv.msda(x=train_dataX5[,(800*(i-1)+1):(800*i)],y=train_dataY5[,i],nfolds=5,lambda.opt ="max")$lambda == lambda_min5[i])
  predd5[,i]<-predict(cv.msda(x=train_dataX5[,(800*(i-1)+1):(800*i)],y=train_dataY5[,i],nfolds=5,lambda.opt ="max")$msda.fit,test_dataX5[,(800*(i-1)+1):(800*i)])[,id_min5[i]]
  nonzero_coef5[i]<-cv.msda(x=train_dataX5[,(800*(i-1)+1):(800*i)],y=train_dataY5[,i],nfolds=5,lambda.opt ="max")$msda.fit$df[id_min5[i]]
}
count_error5<-rep(0,50)
# error<-matrix(0,nrow=1,ncol=50)
for(j in 1:50){
  for(k in 1:90){
    if(predd5[k,j] != test_dataY5[k,j]){
      count_error5[j]<-count_error5[j]+1
    }
  }
}
count_error_rate5<-count_error5/90
median_error5<-summary(count_error_rate5)[3]
sd_error5<-sd(count_error_rate5)
C5_id<-which(nonzero_coef5!=8)
C5<-nonzero_coef5[C5_id]
IC5<-C5-8

# l1 penalized LDA
library(penalizedLDA)
# Xlda<-train_dataX5[,1:800]
# Ylda<-train_dataY5[,1]
# plda.out<-PenalizedLDA.cv(Xlda,Ylda,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))
# plda.out1<-PenalizedLDA(Xlda,Ylda,xte=test_dataX5[,1:800],lambda=plda.out$bestlambda,K=plda.out$bestK)
# print(table(plda.out1$ypred[,plda.out1$K],test_dataY5[,1]))

# plda_cv11<-PenalizedLDA.cv(Xlda,Ylda,type="standard",nfold=5,lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))
##the same best lambda
predict_cv<-predict.penlda(plda.out1,xte=test_dataX5[,1:800])

lambda_best<-rep(0,50)
best_id<-rep(0,50)
predict_plda<-matrix(0,nrow=90,ncol=50)
K_best<-rep(0,50)
nonzero_plda5<-matrix(0,nrow=50,ncol=3)
for(k in 1:50){
  lambda_best[k]<-PenalizedLDA.cv(train_dataX5[,(800*(k-1)+1):(800*k)],train_dataY5[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestlambda
  best_id[k]<-which(PenalizedLDA.cv(train_dataX5[,(800*(k-1)+1):(800*k)],train_dataY5[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$lambda==lambda_best[k])
  K_best[k]<-PenalizedLDA.cv(train_dataX5[,(800*(k-1)+1):(800*k)],train_dataY5[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestK
  predict_plda[,k]<-PenalizedLDA(train_dataX5[,(800*(k-1)+1):(800*k)],train_dataY5[,k],xte=test_dataX5[,(800*(k-1)+1):(800*k)],lambda=lambda_best[k],K=K_best[k])$ypred[,K_best[k]]
  nonzero_plda5[k,]<-PenalizedLDA.cv(train_dataX5[,(800*(k-1)+1):(800*k)],train_dataY5[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$nnonzero[best_id[k],]
  
}
plda_error<-rep(0,50)
for(j in 1:50){
  for(k in 1:90){
    if(predict_plda[k,j] != test_dataY5[k,j]){
      plda_error[j]<-plda_error[j]+1
    }
  }
}
plda_error_rate5<-plda_error/90
View(nonzero_plda5)
C5p_id<-which(nonzero_plda5[,3]!=8)
C5p<-nonzero_plda5[C5p_id,3]
IC5p<-C5p-8
