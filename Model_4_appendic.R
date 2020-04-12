##### Model4
#sigma
CS_4<- matrix(0.8, ncol = 800, nrow = 800)
diag(CS_4) <- 1

#beta
beta_41<- c(1+runif(4,-0.25,0.25), rep(0,796))
beta_42<- c(2+runif(4,-0.25,0.25), rep(0,796))
beta_43<- c(3+runif(4,-0.25,0.25), rep(0,796))
beta_44<- c(4+runif(4,-0.25,0.25), rep(0,796))

#mu
mu_41<-CS_4%*%beta_41
mu_42<-CS_4%*%beta_42
mu_43<-CS_4%*%beta_43
mu_44<-CS_4%*%beta_44

#data
dataX_41<-mvrnorm(n=75,mu=mu_41,Sigma=CS_4)
dataY_41<-matrix(1,nrow=75,ncol=1)
dataX_42<-mvrnorm(n=75,mu=mu_42,Sigma=CS_4)
dataY_42<-matrix(2,nrow=75,ncol=1)
dataX_43<-mvrnorm(n=75,mu=mu_43,Sigma=CS_4)
dataY_43<-matrix(3,nrow=75,ncol=1)
dataX_44<-mvrnorm(n=75,mu=mu_44,Sigma=CS_4)
dataY_44<-matrix(4,nrow=75,ncol=1)
dataX_4<-rbind(dataX_41,dataX_42,dataX_43,dataX_44)
dataY_4<-rbind(dataY_41,dataY_42,dataY_43,dataY_44)
data_4<-cbind(dataX_4,dataY_4)

###train data
Matrix_train4 <- matrix(0,nrow=210,ncol=50)
for(i in 1:50){
  Matrix_train4[,i]<-sample(nrow(data_4), 0.7*nrow(data_4))
}

train_dataX4 <- matrix(0,nrow=210,ncol=40000)
for(j in 1:50){
  train_dataX4[,(800*(j-1)+1):(800*j)]<-dataX_4[Matrix_train4[,j],]
}

train_dataY4<-matrix(0,nrow=210,ncol=50)
for(k in 1:50){
  train_dataY4[,k]<-dataY_4[Matrix_train4[,k]]
}


####test data
test_dataX4 <- matrix(0,nrow=90,ncol=40000)
for(m in 1:50){
  test_dataX4[,(800*(m-1)+1):(800*m)] <- dataX_4[-(Matrix_train5[,m]),]
}
test_dataY4<-matrix(0,nrow=90,ncol=50)
for(l in 1:50){
  test_dataY4[,l] <- dataY_4[-(Matrix_train4[,l])]
}

##fit the data
lambda_min4 <- rep(0,50)
id_min4 <- rep(0,50)
predd4 <- matrix(0,nrow=90,ncol=50)
nonzero_coef4 <- rep(0,50)
for(i in 1:50){
  lambda_min4[i]<-cv.msda(x=train_dataX4[,(800*(i-1)+1):(800*i)],y=train_dataY4[,i],nfolds=5,lambda.opt ="max")$lambda.min
  id_min4[i]<-which(cv.msda(x=train_dataX4[,(800*(i-1)+1):(800*i)],y=train_dataY4[,i],nfolds=5,lambda.opt ="max")$lambda == lambda_min4[i])
  predd4[,i]<-predict(cv.msda(x=train_dataX4[,(800*(i-1)+1):(800*i)],y=train_dataY4[,i],nfolds=5,lambda.opt ="max")$msda.fit,test_dataX4[,(800*(i-1)+1):(800*i)])[,id_min4[i]]
  nonzero_coef4[i]<-cv.msda(x=train_dataX4[,(800*(i-1)+1):(800*i)],y=train_dataY4[,i],nfolds=5,lambda.opt ="max")$msda.fit$df[id_min4[i]]
}
count_error4<-rep(0,50)
# error<-matrix(0,nrow=1,ncol=50)
for(j in 1:50){
  for(k in 1:90){
    if(predd4[k,j] != test_dataY4[k,j]){
      count_error4[j]<-count_error4[j]+1
    }
  }
}
count_error_rate4<-count_error4/90
median_error4<-summary(count_error_rate4)[3]
sd_error4<-sd(count_error_rate4)
median(nonzero_coef4)
sd(nonzero_coef4)
C4_id<-which(nonzero_coef4!=4)
C4 <-nonzero_coef4[C4_id]

#####
lambda_best<-rep(0,50)
best_id<-rep(0,50)
predict_plda<-matrix(0,nrow=90,ncol=50)
K_best<-rep(0,50)
nonzero_plda4<-matrix(0,nrow=50,ncol=3)
for(k in 1:50){
  lambda_best[k]<-PenalizedLDA.cv(train_dataX4[,(800*(k-1)+1):(800*k)],train_dataY4[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestlambda
  best_id[k]<-which(PenalizedLDA.cv(train_dataX4[,(800*(k-1)+1):(800*k)],train_dataY4[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$lambda==lambda_best[k])
  K_best[k]<-PenalizedLDA.cv(train_dataX4[,(800*(k-1)+1):(800*k)],train_dataY4[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$bestK
  predict_plda[,k]<-PenalizedLDA(train_dataX4[,(800*(k-1)+1):(800*k)],train_dataY4[,k],xte=test_dataX4[,(800*(k-1)+1):(800*k)],lambda=lambda_best[k],K=K_best[k])$ypred[,K_best[k]]
  nonzero_plda4[k,]<-PenalizedLDA.cv(train_dataX4[,(800*(k-1)+1):(800*k)],train_dataY4[,k],nfold=5,type="standard",lambdas=c(1e-4,1e-3,1e-2,0.1,1,10))$nnonzero[best_id[k],]
  
}
plda_error<-rep(0,50)
for(j in 1:50){
  for(k in 1:90){
    if(predict_plda[k,j] != test_dataY4[k,j]){
      plda_error[j]<-plda_error[j]+1
    }
  }
}
plda_error4<-plda_error/90
plda_error_rate4<-summary(plda_error4)[3]

##
