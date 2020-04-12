###### model3
library(MASS)
# aaa <- matrix(c(10,3,3,2),2,2)
# VVV<-mvrnorm(n=75, rep(0, 2),aaa)
CS_structure<-function(matrix_sample,alpha,p){
  for(j in 1:p){
    for(k in 1:p){
      if(k == j){
        matrix_sample[j,k]=1
      }else {
        matrix_sample[j,k]=alpha
      }
    }
  }
  return(matrix_sample)
}
beta_31<-c(runif(4,-0.25,0.25)+1,rep(0,796))
beta_32<-c(runif(4,-0.25,0.25)+2,rep(0,796))
beta_33<-c(runif(4,-0.25,0.25)+3,rep(0,796))
beta_34<-c(runif(4,-0.25,0.25)+4,rep(0,796))
CS_initial<-matrix(0,nrow=800,ncol=800)
CS_3<-CS_structure(CS_initial,0.5,800)
mu_31<-CS_3%*%beta_31
mu_32<-CS_3%*%beta_32
mu_33<-CS_3%*%beta_33
mu_34<-CS_3%*%beta_34
dataX_31<-mvrnorm(n=75,mu=mu_31,Sigma=CS_3)
dataY_31<-matrix(1,nrow=75,ncol=1)
dataX_32<-mvrnorm(n=75,mu=mu_32,Sigma=CS_3)
dataY_32<-matrix(2,nrow=75,ncol=1)
dataX_33<-mvrnorm(n=75,mu=mu_33,Sigma=CS_3)
dataY_33<-matrix(3,nrow=75,ncol=1)
dataX_34<-mvrnorm(n=75,mu=mu_34,Sigma=CS_3)
dataY_34<-matrix(4,nrow=75,ncol=1)
# mean(data_31[,1])
dataX_3<-rbind(dataX_31,dataX_32,dataX_33,dataX_34)
dataY_3<-rbind(dataY_31,dataY_32,dataY_33,dataY_34)
data_3<-cbind(dataX_3,dataY_3)
######train
Matrix_train3<-matrix(0,nrow=210,ncol=50)
for(i in 1:50){
  Matrix_train3[,i]<-sample(nrow(data_3),0.7*nrow(data_3))
}
train_dataX3<-matrix(0,nrow=210,ncol=40000)
for(j in 1:50){
  train_dataX3[,(800*(j-1)+1):(800*j)]<-dataX_3[Matrix_train3[,j],]
}
# train_dataX[,1:127]<-X_GDS1615[Matrix_train[1],]
train_dataY3<-matrix(0,nrow=210,ncol=50)
for(k in 1:50){
  train_dataY3[,k]<-dataY_3[Matrix_train3[,k]]
}
class(train_dataY)
#####test
test_dataX3<-matrix(0,nrow=90,ncol=40000)
for(m in 1:50){
  test_dataX3[,(800*(m-1)+1):(800*m)]<-dataX_3[-(Matrix_train3[,m]),]
}
test_dataY3<-matrix(0,nrow=90,ncol=50)
for(l in 1:50){
  test_dataY3[,l]<-dataY_3[-(Matrix_train3[,l])]
}
##fit the data
# listfit<-vector("list",100)
lambda_min3<-rep(0,50)
id_min3<-rep(0,50)
predd3<-matrix(0,nrow=90,ncol=50)
nonzero_coef3<-rep(0,50)
for(i in 1:50){
  # listfit[i]<-cv.msda(x=train_dataX[,(127*(i-1)+1):(127*i)],y=train_dataY[,i],nfolds=5,lambda.opt ="max")$msda.fit
  lambda_min3[i]<-cv.msda(x=train_dataX3[,(800*(i-1)+1):(800*i)],y=train_dataY3[,i],nfolds=5,lambda.opt ="max")$lambda.min
  id_min3[i]<-which(cv.msda(x=train_dataX3[,(800*(i-1)+1):(800*i)],y=train_dataY3[,i],nfolds=5,lambda.opt ="max")$lambda == lambda_min3[i])
  predd3[,i]<-predict(cv.msda(x=train_dataX3[,(800*(i-1)+1):(800*i)],y=train_dataY3[,i],nfolds=5,lambda.opt ="max")$msda.fit,test_dataX3[,(800*(i-1)+1):(800*i)])[,id_min3[i]]
  nonzero_coef3[i]<-cv.msda(x=train_dataX3[,(800*(i-1)+1):(800*i)],y=train_dataY3[,i],nfolds=5,lambda.opt ="max")$msda.fit$df[id_min3[i]]
}
count_error3<-rep(0,50)
# error<-matrix(0,nrow=1,ncol=50)
for(j in 1:50){
  for(k in 1:90){
    if(predd3[k,j] != test_dataY3[k,j]){
      count_error3[j]<-count_error3[j]+1
    }
  }
}
count_error_rate3<-count_error/90
median_error3<-summary(count_error_rate3)[3]
sd_error3<-sd(count_error_rate3)
# nonzero_coef<-rep(0,50)
# for(k in 1:50){
#   nonzero_coef[k]<-cv.msda(x=train_dataX[,(127*(k-1)+1):(127*k)],y=train_dataY[,k],nfolds=5,lambda.opt ="max")$msda.fit$df[id_min[k]]
# }
C31_id<-which(nonzero_coef3!=4)
C31<-nonzero_coef3[C31_id]

###l1 penalized LDA
lambda_best3<-rep(0,50)
best_id3<-rep(0,50)
predict_plda3<-matrix(0,nrow=90,ncol=50)
K_best3<-rep(0,50)
nonzero_plda3<-matrix(0,nrow=50,ncol=3)
for(k in 1:50){
  lambda_best3[k]<-PenalizedLDA.cv(train_dataX3[,(800*(k-1)+1):(800*k)],train_dataY3[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestlambda
  best_id3[k]<-which(PenalizedLDA.cv(train_dataX3[,(800*(k-1)+1):(800*k)],train_dataY3[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$lambda==lambda_best[k])
  K_best3[k]<-PenalizedLDA.cv(train_dataX3[,(800*(k-1)+1):(800*k)],train_dataY3[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestK
  predict_plda3[,k]<-PenalizedLDA(train_dataX3[,(800*(k-1)+1):(800*k)],train_dataY3[,k],xte=test_dataX3[,(800*(k-1)+1):(800*k)],lambda=lambda_best3[k],K=K_best3[k])$ypred[,K_best3[k]]
  nonzero_plda3[k,]<-PenalizedLDA.cv(train_dataX3[,(800*(k-1)+1):(800*k)],train_dataY3[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$nnonzero[best_id3[k],]
}
plda_error3<-rep(0,50)
for(j in 1:50){
  for(k in 1:90){
    if(predict_plda3[k,j] != test_dataY3[k,j]){
      plda_error3[j]<-plda_error3[j]+1
    }
  }
}
plda_error_rate3<-plda_error3/90
View(nonzero_plda3)



