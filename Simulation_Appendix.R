##### Application on IBD dataset
library(msda)
# library(CVTuningCov)
# p<-800
# Sigma <- AR1(p, rho=0.5)
# Sigma[100,401]

X_GDS1615<-GDS1615$x
Y_GDS1615<-GDS1615$y
# X_GDS1615

library(msda)
data_GDS1615<-data.frame(X_GDS1615,Y_GDS1615)

######
Matrix_train<-matrix(0,nrow=88,ncol=50)
for(i in 1:50){
  Matrix_train[,i]<-sample(nrow(data_GDS1615),0.7*nrow(data_GDS1615))
}
train_dataX<-matrix(0,nrow=88,ncol=6350)
for(j in 1:50){
  train_dataX[,(127*(j-1)+1):(127*j)]<-X_GDS1615[Matrix_train[,j],]
}
# train_dataX[,1:127]<-X_GDS1615[Matrix_train[1],]
train_dataY<-matrix(0,nrow=88,ncol=50)
for(k in 1:50){
  train_dataY[,k]<-Y_GDS1615[Matrix_train[,k]]
}
class(train_dataY)
#####
test_dataX<-matrix(0,nrow=39,ncol=6350)
for(m in 1:50){
  test_dataX[,(127*(m-1)+1):(127*m)]<-X_GDS1615[-(Matrix_train[,m]),]
}
test_dataY<-matrix(0,nrow=39,ncol=50)
for(l in 1:50){
  test_dataY[,l]<-Y_GDS1615[-(Matrix_train[,l])]
}

# listfit<-vector("list",100)
lambda_min<-rep(0,50)
id_min<-rep(0,50)
predd<-matrix(0,nrow=39,ncol=50)
for(i in 1:50){
  # listfit[i]<-cv.msda(x=train_dataX[,(127*(i-1)+1):(127*i)],y=train_dataY[,i],nfolds=5,lambda.opt ="max")$msda.fit
  lambda_min[i]<-cv.msda(x=train_dataX[,(127*(i-1)+1):(127*i)],y=train_dataY[,i],nfolds=5,lambda.opt ="max")$lambda.min
  id_min[i]<-which(cv.msda(x=train_dataX[,(127*(i-1)+1):(127*i)],y=train_dataY[,i],nfolds=5,lambda.opt ="max")$lambda == lambda_min[i])
  predd[,i]<-predict(cv.msda(x=train_dataX[,(127*(i-1)+1):(127*i)],y=train_dataY[,i],nfolds=5,lambda.opt ="max")$msda.fit,test_dataX[,(127*(i-1)+1):(127*i)])[,id_min[i]]
}
count_error<-rep(0,50)
# error<-matrix(0,nrow=1,ncol=50)
for(j in 1:50){
  for(k in 1:39){
    if(predd[k,j] != test_dataY[k,j]){
      count_error[j]<-count_error[j]+1
    }
  }
}
count_error_rate<-count_error/39
median_error<-summary(count_error_rate)[3]
sd_error<-sd(count_error_rate)
nonzero_coef<-rep(0,50)
for(k in 1:50){
  nonzero_coef[k]<-cv.msda(x=train_dataX[,(127*(k-1)+1):(127*k)],y=train_dataY[,k],nfolds=5,lambda.opt ="max")$msda.fit$df[id_min[k]]
}
nonzero_coef
median_nonzerocoef<-summary(nonzero_coef)[3]
sd_nonzerocoef<-sd(nonzero_coef)

####l1 penalized 
best_minp<-rep(0,50)
predict_pldap<-matrix(0,nrow=39,ncol=50)
K_bestminp<-rep(0,50)
nonzero_pldap<-rep(0,50)
for(k in 1:50){
  # lambda_minp[k]<-PenalizedLDA.cv(train_dataX[,(127*(k-1)+1):(127*k)],train_dataY[,k],nfold=5,type="standard",lambdas=c(-0.0001,-0.001,-0.003700,-0.01,0.022225,0.1,1))$bestlambda
  best_minp[k]<-which(PenalizedLDA.cv(train_dataX[,(127*(k-1)+1):(127*k)],train_dataY[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1))$lambda==0.1)
  K_bestminp[k]<-PenalizedLDA.cv(train_dataX[,(127*(k-1)+1):(127*k)],train_dataY[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1))$bestK
  predict_pldap[,k]<-PenalizedLDA(train_dataX[,(127*(k-1)+1):(127*k)],train_dataY[,k],xte=test_dataX[,(127*(k-1)+1):(127*k)],lambda=0.1,K=K_bestminp[k])$ypred
  nonzero_pldap[k]<-PenalizedLDA.cv(train_dataX[,(127*(k-1)+1):(127*k)],train_dataY[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1))$nnonzero[best_minp[k]]
}
plda_errorp<-rep(0,50)
for(j in 1:50){
  for(k in 1:39){
    if(predict_pldap[k,j] != test_dataY[k,j]){
      plda_errorp[j]<-plda_errorp[j]+1
    }
  }
}
plda_error_ratep<-plda_errorp/39
