
library(Biobase)
library(ALL)
library(genefilter)
library(DMwR)
library(class)
library(lattice)
library(Hmisc)
library(randomForest)
load('/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/code/20170329/ana_33/knn.Rdata')
a<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge_1.txt")
rownames(a)<-a[,1]
a<-a[,-1]
a<-a[1:100,]
print(length(a[,1]))
print(length(a[1,]))
#dt<-a

dt <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

DSs <- list(dataset(Mut ~ .,dt,'ALL'))

all.trials <- join(knn,by='variants')


#rankSystems(all.trials,top=10,max=T)


#getVariant('knn.v2',all.trials)


bestknn.loocv <- function(form,train,test,...) {
  require(Biobase,quietly=T)
  require(randomForest,quietly=T)
  cat('=')
  tgt <- as.character(form[[2]])
  tgtCol <- which(colnames(train)==tgt)
  # Anova filtering
  f <- Anova(train[,tgt],p=0.01)
  ff <- filterfun(f)
  genes <- genefilter(t(train[,-tgtCol]),ff)
  genes <- names(genes)[genes]
  train <- train[,c(tgt,genes)]
  test <- test[,c(tgt,genes)]
  tgtCol <- 1
  # Random Forest filtering
  #rf <- randomForest(form,train,importance=T)
  #imp <- importance(rf)
  #imp <- imp[,ncol(imp)-1]
  #rf.genes <- names(imp)[order(imp,decreasing=T)[1:30]]
  #train <- train[,c(tgt,rf.genes)]
  #test <- test[,c(tgt,rf.genes)]
  # knn prediction
  ps <- kNN(form,train,test,norm=T, 
            norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                            rowIQRs(t(as.matrix(train[,-tgtCol])))),
            k=5,...)
  structure(c(accuracy=ifelse(ps == resp(form,test),100,0)),
            itInfo=list(ps)
           )
}

print("fhdsjhfsdhfkhsdkjhfhasdfhfasfdkjasdhlasflkjf")

rowIQRs <- function(em){rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))}
resTop <- loocv(learner('bestknn.loocv',pars=list()),
                dataset(Mut~.,dt),
                loocvSettings(seed=1234,verbose=F),
                #cvSettings(3,10,seed=1234),
                itsInfo=T)


attr(resTop,'itsInfo')
print("resTop")
print(resTop)
print("abcaaaaaaaaaaaaaaaaaaaaaaaaa")

#dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)
dt <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

preds <- unlist(attr(resTop,'itsInfo'))
print(preds)
print(dt$Mut)
table(preds,dt$Mut)
print(table(preds,dt$Mut))
