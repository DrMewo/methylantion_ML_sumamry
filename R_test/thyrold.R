library(Biobase)
library(ALL)
library(genefilter)
library(DMwR)
library(class)
library(lattice)
library(Hmisc)
library(randomForest)



#data(ALL)
#ALL
#pD <- phenoData(ALL)
#varMetadata(pD)
#table(ALL$BT)
#table(ALL$mol.biol)
#table(ALL$BT,ALL$mol.bio)
#featureNames(ALL)[1:10]
#sampleNames(ALL)[1:5]
#tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & 
#                   ALL$mol.bio %in% levels(ALL$mol.bio)[1:4])
#ALLb <- ALL[,tgt.cases]
#ALLb
#ALLb$BT <- factor(ALLb$BT)
#ALLb$mol.bio <- factor(ALLb$mol.bio)


###################################################
### Exploring the data set
###################################################
#es <- exprs(ALLb)


#ALLb <- nsFilter(ALLb,
#                 var.func=IQR,var.cutoff=IQR(as.vector(es))/5, 
#                 feature.exclude="^AFFX")
#ALLb <- ALLb$eset
# the data set
#featureNames(ALLb) <- make.names(featureNames(ALLb))
#dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)
#dt<-dt[,c(3800:3943)]


#######################

a<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge_1.txt")
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
print(alength)
print("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"colorectal"}else{y<-"other"})
#dt<-a
blength<-length(a[1,])
print(blength)
print("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB")
a[which(a[,alength]=='c73'),blength]<-"thyrold"
a[which(a[,alength]!='c73'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)

#print(head(a[,length(a[1,])]))

a1<-a[which(a[,alength]=="thyrold"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a2_length<-a2_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]
a<-rbind(a1,a2)

dt <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

#print(a[,length(a[1,])])

#print("a")





######################


############data_in_put


getVarsSet <- function(cluster,nvars=30,seed=NULL,verb=F) 
{
  if (!is.null(seed)) set.seed(seed)

  cls <- cutree(cluster,nvars)
  tots <- table(cls)
  vars <- c()
  vars <- sapply(1:nvars,function(clID)
    {
      if (!length(tots[clID])) stop('Empty cluster! (',clID,')')
      x <- sample(1:tots[clID],1)
      names(cls[cls==clID])[x]
    })
  if (verb)  structure(vars,clusMemb=cls,clusTots=tots)
  else       vars
}


rpart.loocv <- function(form,train,test,...) {
  require(rpart,quietly=T)
  m <- rpart(form,train,...)
  p <- predict(m,test,type='class')
  c(accuracy=ifelse(p == resp(form,test),100,0))
}



rowIQRs <- function(em){rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))}

###plot(rowMedians(es),rowIQRs(es), xlab='Median expression level', ylab='IQR expression level', main='Main Characteristics of Genes Expression Levels')


kNN <- function(form,train,test,norm=T,norm.stats=NULL,...) {
  require(class,quietly=TRUE)
  tgtCol <- which(colnames(train)==as.character(form[[2]]))
  if (norm) {
    if (is.null(norm.stats)) tmp <- scale(train[,-tgtCol],center=T,scale=T)
    else tmp <- scale(train[,-tgtCol],center=norm.stats[[1]],scale=norm.stats[[2]])
    train[,-tgtCol] <- tmp
    ms <- attr(tmp,"scaled:center")
    ss <- attr(tmp,"scaled:scale")
    test[,-tgtCol] <- scale(test[,-tgtCol],center=ms,scale=ss)
  }
  knn(train[,-tgtCol],test[,-tgtCol],train[,tgtCol],...)
}


vars <- list()
vars$randomForest <- list(ntree=c(500,750,100),
                          mtry=c(5,15,30),
                          fs.meth=list(list('all'),
                                       list('rf',30),
                                       list('varclus',30,50)))
vars$svm <- list(cost=c(1,100,500),
                 gamma=c(0.01,0.001,0.0001),
                 fs.meth=list(list('all'),
                              list('rf',30),
                              list('varclus',30,50)))
vars$knn <- list(k=c(3,5,7,11),
                 norm=c(T,F),
                 fs.meth=list(list('all'),
                              list('rf',30),
                              list('varclus',30,50)))

print("b")
vars <- list()
vars$randomForest <- list(ntree=c(500,750,100),
                          mtry=c(5,15,30),
                          fs.meth=list(
                                       list('ALL')))
vars$svm <- list(cost=c(1,100,500),
                 gamma=c(0.01,0.001,0.0001),
                 fs.meth=list(
                              list('ALL')))
vars$knn <- list(k=c(3,5,7,11),
                 norm=c(T,F),
                 fs.meth=list(
                              list('ALL')))                             
                              
                              
varsEnsembles <- function(tgt,train,test,
                          varsSets,
                          baseLearner,blPars,
                          verb=F)                        
{
  preds <- matrix(NA,ncol=length(varsSets),nrow=NROW(test))
  for(v in seq(along=varsSets)) {
    if (baseLearner=='knn')
      preds[,v] <- knn(train[,varsSets[[v]]],
                       test[,varsSets[[v]]],
                       train[,tgt],blPars)
    else {
      m <- do.call(baseLearner,
                   c(list(as.formula(paste(tgt,
                                           paste(varsSets[[v]],
                                                 collapse='+'),
                                           sep='~')),
                          train[,c(tgt,varsSets[[v]])]),
                     blPars)
                   )
      if (baseLearner == 'randomForest')
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])],
                                  type='response'))
      else
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])]))
    }}
  ps <- apply(preds,1,function(x)
                  {levels(factor(x))[which.max(table(factor(x)))]})
  ps <- factor(ps,
               levels=1:nlevels(train[,tgt]),
               labels=levels(train[,tgt]))
  if (verb) structure(ps,ensemblePreds=preds) else ps
}


genericModel <- function(form,train,test,
                         learner,
                         fs.meth,
                         ...)
  {
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

    # Specific filtering 
    if (fs.meth[[1]]=='varclus') {
      require(Hmisc,quietly=T)
      print("a")
      v <- varclus(as.matrix(train[,-tgtCol]))
      print(v)
      #v <- varclus(as.matrix(train))
      VSs <- lapply(1:fs.meth[[3]],function(x)
                    getVarsSet(v$hclust,nvars=fs.meth[[2]]))
      print(VSs)
      pred <- varsEnsembles(tgt,train,test,VSs,learner,list(...))

    } else {
      if (fs.meth[[1]]=='rf') {
        require(randomForest,quietly=T)
        rf <- randomForest(form,train,importance=T)
        imp <- importance(rf)
        imp <- imp[,ncol(imp)-1]
        rf.genes <- names(imp)[order(imp,decreasing=T)[1:fs.meth[[2]]]]
        train <- train[,c(tgt,rf.genes)]
        test <- test[,c(tgt,rf.genes)]
      }
      if (learner == 'knn') 
        pred <- kNN(form,
             train,
             test,
             norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                             rowIQRs(t(as.matrix(train[,-tgtCol])))),
             ...)
      else {
        model <- do.call(learner,c(list(form,train),list(...)))
        pred <- if (learner != 'randomForest') predict(model,test)
                else predict(model,test,type='response')
      }
    }
    c(accuracy=ifelse(pred == resp(form,test),100,0))
  }


require(class,quietly=TRUE)
require(randomForest,quietly=TRUE)
require(e1071,quietly=TRUE)





print("c")

DSs <- list(dataset(Mut ~ .,dt,'ALL'))
#DSs <- list(dataset(primarg_diagnosis ~ .,dt,'ALL'))
# The learners to evaluate
TODO <- c('knn','svm','randomForest')
for(td in TODO) {
  assign(td,
         experimentalComparison(
              DSs,
              c(
                do.call('variants',
                        c(list('genericModel',learner=td),
                          vars[[td]],
                          varsRootName=td))
                ),
               cvSettings(3,10,1234)
                                )
         )
  file_path = '/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model2/thyrold/'
  file_path = paste(file_path,td,sep='')
  file_path = paste(file_path,'Rdata', sep = '.')
  #save(list=td,file=paste(td,'Rdata',sep='.'))
  save(list=td,file=file_path)
}
