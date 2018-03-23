library(Biobase)
library(ALL)
library(genefilter)
library(DMwR)
library(class)
library(lattice)
library(Hmisc)
library(randomForest)
library(e1071)

#######################构建knn新的函数##################

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
  
}

#######################构建数据#########################

#######################构建数据#########################

a_zp<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge_1.txt")



######################选择跑哪个分析##################

adrenal_gland<-TRUE
colorectal<-TRUE
lung<-TRUE
bladder<-TRUE
breast<-TRUE
cervix<-TRUE
esophagus<-TRUE
kidney<-TRUE
liver<-TRUE
pancreas<-TRUE
prostate<-TRUE
stomach<-TRUE
thyrold<-TRUE
uterus<-TRUE



if(adrenal_gland){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"colorectal"}else{y<-"other"})
#dt<-a
blength<-length(a[1,])
a[which(a[,alength]!='c72'),blength]<-"other"
a[which(a[,alength]!='c74'),blength]<-"other"
a[which(a[,alength]!='c75'),blength]<-"other"
a[which(a[,alength]!='c76'),blength]<-"other"
a[which(a[,alength]=='c72'|a[,alength]=='c74'|a[,alength]=='c75'|a[,alength]=='c76'),blength]<-"adrenal_gland"
#a[which(a[,alength]!='c22'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="adrenal_gland"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]!='c72'),bxlength]<-1
ax[which(ax[,axlength]!='c74'),bxlength]<-1
ax[which(ax[,axlength]!='c75'),bxlength]<-1
ax[which(ax[,axlength]!='c76'),bxlength]<-1
ax[which(ax[,axlength]=='c72'|ax[,axlength]=='c74'|ax[,axlength]=='c75'|ax[,axlength]=='c76'),bxlength]<-0

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_adrenal_gland_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
#test_two
#test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 5, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 5, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 5, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 5, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_adrenal_gland <- svm(x_dt_all,y_dt_all, gamma = 0.001, cost=1)
save(model_dt_all_adrenal_gland,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_big_chr.Rdata")


print(predict(model_dt_all_adrenal_gland,test_one))
print(predict(model_dt_all_adrenal_gland,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_adrenal_gland <- svm(x_dt_samll,y_dt_samll, gamma = 0.001, cost=1)
save(model_dt_samll_adrenal_gland,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_samll_chr.Rdata")


print(predict(model_dt_samll_adrenal_gland,test_one))
print(predict(model_dt_samll_adrenal_gland,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_adrenal_gland <- svm(x_dxt_all,y_dxt_all, gamma = 0.001, cost=1)
save(model_dxt_all_adrenal_gland,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_big_num.Rdata")


print(predict(model_dxt_all_adrenal_gland,test_one))
print(predict(model_dxt_all_adrenal_gland,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_adrenal_gland <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.001, cost=1)
save(model_dxt_samll_adrenal_gland,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_samll_num.Rdata")

print(predict(model_dxt_samll_adrenal_gland,test_one))
print(predict(model_dxt_samll_adrenal_gland,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_adrenal_gland_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_adrenal_gland_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_big_chr.Rdata")

print(predict(model_dt_all_adrenal_gland_rf,test_one))
print(predict(model_dt_all_adrenal_gland_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_adrenal_gland_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_adrenal_gland_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_samll_chr.Rdata")

print(predict(model_dt_samll_adrenal_gland_rf,test_one))
print(predict(model_dt_samll_adrenal_gland_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_adrenal_gland_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_adrenal_gland_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_adrenal_gland_rf,test_one))
print(predict(model_dxt_all_adrenal_gland_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_adrenal_gland_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_adrenal_gland_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_samll_num.Rdata")

print(predict(model_dxt_samll_adrenal_gland_rf,test_one))
print(predict(model_dxt_samll_adrenal_gland_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}


print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-1)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(colorectal){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"colorectal"}else{y<-"other"})
#dt<-a

###colorectal_colorectal
blength<-length(a[1,])
a[which(a[,alength]!='c18'),blength]<-"other"
a[which(a[,alength]!='c19'),blength]<-"other"
a[which(a[,alength]!='c20'),blength]<-"other"
a[which(a[,alength]=='c18'|a[,alength]=='c19'|a[,alength]=='c20'),blength]<-"colorectal"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="colorectal"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]!='c18'),bxlength]<-1
ax[which(ax[,axlength]!='c19'),bxlength]<-1
ax[which(ax[,axlength]!='c20'),bxlength]<-1
ax[which(ax[,axlength]=='c18'|ax[,axlength]=='c19'|ax[,axlength]=='c20'),bxlength]<-0

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_colorectal_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]


save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 7, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 7, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 7, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 7, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_colorectal <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_colorectal,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_big_chr.Rdata")


print(predict(model_dt_all_colorectal,test_one))
print(predict(model_dt_all_colorectal,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_colorectal <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_colorectal,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_samll_chr.Rdata")


print(predict(model_dt_samll_colorectal,test_one))
print(predict(model_dt_samll_colorectal,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_colorectal <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_colorectal,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_big_num.Rdata")


print(predict(model_dxt_all_colorectal,test_one))
print(predict(model_dxt_all_colorectal,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_colorectal <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_colorectal,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_samll_num.Rdata")

print(predict(model_dxt_samll_colorectal,test_one))
print(predict(model_dxt_samll_colorectal,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_colorectal_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_colorectal_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_big_chr.Rdata")

print(predict(model_dt_all_colorectal_rf,test_one))
print(predict(model_dt_all_colorectal_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_colorectal_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_colorectal_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_samll_chr.Rdata")

print(predict(model_dt_samll_colorectal_rf,test_one))
print(predict(model_dt_samll_colorectal_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_colorectal_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_colorectal_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_colorectal_rf,test_one))
print(predict(model_dxt_all_colorectal_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_colorectal_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_colorectal_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_samll_num.Rdata")

print(predict(model_dxt_samll_colorectal_rf,test_one))
print(predict(model_dxt_samll_colorectal_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-2)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(lung){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"lung"}else{y<-"other"})
#dt<-a

###lung_lung
blength<-length(a[1,])
a[which(a[,alength]=='c34'),blength]<-"lung"
a[which(a[,alength]!='c34'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="lung"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c34'),bxlength]<-0
ax[which(ax[,axlength]!='c34'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_lung_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 3, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 3, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 3, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 3, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_lung <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_lung,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_big_chr.Rdata")


print(predict(model_dt_all_lung,test_one))
print(predict(model_dt_all_lung,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_lung <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_lung,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_samll_chr.Rdata")


print(predict(model_dt_samll_lung,test_one))
print(predict(model_dt_samll_lung,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_lung <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_lung,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_big_num.Rdata")


print(predict(model_dxt_all_lung,test_one))
print(predict(model_dxt_all_lung,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_lung <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_lung,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_samll_num.Rdata")

print(predict(model_dxt_samll_lung,test_one))
print(predict(model_dxt_samll_lung,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_lung_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_lung_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_big_chr.Rdata")

print(predict(model_dt_all_lung_rf,test_one))
print(predict(model_dt_all_lung_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_lung_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_lung_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_samll_chr.Rdata")

print(predict(model_dt_samll_lung_rf,test_one))
print(predict(model_dt_samll_lung_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_lung_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_lung_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_lung_rf,test_one))
print(predict(model_dxt_all_lung_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_lung_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_lung_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_samll_num.Rdata")

print(predict(model_dxt_samll_lung_rf,test_one))
print(predict(model_dxt_samll_lung_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-3)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(bladder){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"bladder"}else{y<-"other"})
#dt<-a

###bladder_bladder
blength<-length(a[1,])
a[which(a[,alength]=='c67'),blength]<-"bladder"
a[which(a[,alength]!='c67'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="bladder"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c67'),bxlength]<-0
ax[which(ax[,axlength]!='c67'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_bladder_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 7, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 7, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 7, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 7, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_bladder <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_bladder,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_big_chr.Rdata")


print(predict(model_dt_all_bladder,test_one))
print(predict(model_dt_all_bladder,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_bladder <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_bladder,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_samll_chr.Rdata")


print(predict(model_dt_samll_bladder,test_one))
print(predict(model_dt_samll_bladder,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_bladder <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_bladder,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_big_num.Rdata")


print(predict(model_dxt_all_bladder,test_one))
print(predict(model_dxt_all_bladder,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_bladder <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_bladder,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_samll_num.Rdata")

print(predict(model_dxt_samll_bladder,test_one))
print(predict(model_dxt_samll_bladder,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_bladder_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_bladder_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_big_chr.Rdata")

print(predict(model_dt_all_bladder_rf,test_one))
print(predict(model_dt_all_bladder_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_bladder_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_bladder_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_samll_chr.Rdata")

print(predict(model_dt_samll_bladder_rf,test_one))
print(predict(model_dt_samll_bladder_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_bladder_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_bladder_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_bladder_rf,test_one))
print(predict(model_dxt_all_bladder_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_bladder_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_bladder_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_samll_num.Rdata")

print(predict(model_dxt_samll_bladder_rf,test_one))
print(predict(model_dxt_samll_bladder_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-4)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(breast){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"breast"}else{y<-"other"})
#dt<-a

###breast_breast
blength<-length(a[1,])
a[which(a[,alength]=='c50'),blength]<-"breast"
a[which(a[,alength]!='c50'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="breast"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c50'),bxlength]<-0
ax[which(ax[,axlength]!='c50'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_breast_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 3, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 3, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 3, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 3, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_breast <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_breast,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_big_chr.Rdata")


print(predict(model_dt_all_breast,test_one))
print(predict(model_dt_all_breast,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_breast <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_breast,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_samll_chr.Rdata")


print(predict(model_dt_samll_breast,test_one))
print(predict(model_dt_samll_breast,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_breast <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_breast,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_big_num.Rdata")


print(predict(model_dxt_all_breast,test_one))
print(predict(model_dxt_all_breast,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_breast <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_breast,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_samll_num.Rdata")

print(predict(model_dxt_samll_breast,test_one))
print(predict(model_dxt_samll_breast,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_breast_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_breast_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_big_chr.Rdata")

print(predict(model_dt_all_breast_rf,test_one))
print(predict(model_dt_all_breast_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_breast_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_breast_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_samll_chr.Rdata")

print(predict(model_dt_samll_breast_rf,test_one))
print(predict(model_dt_samll_breast_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_breast_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_breast_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_breast_rf,test_one))
print(predict(model_dxt_all_breast_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_breast_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_breast_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_samll_num.Rdata")

print(predict(model_dxt_samll_breast_rf,test_one))
print(predict(model_dxt_samll_breast_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-5)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(cervix){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"cervix"}else{y<-"other"})
#dt<-a

###cervix_cervix
blength<-length(a[1,])
a[which(a[,alength]=='c53'),blength]<-"cervix"
a[which(a[,alength]!='c53'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="cervix"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c53'),bxlength]<-0
ax[which(ax[,axlength]!='c53'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_cervix_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 5, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 5, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 5, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 5, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_cervix <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_cervix,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_big_chr.Rdata")


print(predict(model_dt_all_cervix,test_one))
print(predict(model_dt_all_cervix,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_cervix <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_cervix,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_samll_chr.Rdata")


print(predict(model_dt_samll_cervix,test_one))
print(predict(model_dt_samll_cervix,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_cervix <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_cervix,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_big_num.Rdata")


print(predict(model_dxt_all_cervix,test_one))
print(predict(model_dxt_all_cervix,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_cervix <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_cervix,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_samll_num.Rdata")

print(predict(model_dxt_samll_cervix,test_one))
print(predict(model_dxt_samll_cervix,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_cervix_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  750, mtry  =  30)
save(model_dt_all_cervix_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_big_chr.Rdata")

print(predict(model_dt_all_cervix_rf,test_one))
print(predict(model_dt_all_cervix_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_cervix_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  750, mtry  =  30)
save(model_dt_samll_cervix_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_samll_chr.Rdata")

print(predict(model_dt_samll_cervix_rf,test_one))
print(predict(model_dt_samll_cervix_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_cervix_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  750, mtry  =  30)
save(model_dxt_all_cervix_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_cervix_rf,test_one))
print(predict(model_dxt_all_cervix_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_cervix_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  750, mtry  =  30)
save(model_dxt_samll_cervix_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_samll_num.Rdata")

print(predict(model_dxt_samll_cervix_rf,test_one))
print(predict(model_dxt_samll_cervix_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}


print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-6)
print(123456789)
print(123456789)
print(123456789)
print(123456789)


if(esophagus){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"esophagus"}else{y<-"other"})
#dt<-a

###esophagus_esophagus
blength<-length(a[1,])
a[which(a[,alength]=='c15'),blength]<-"esophagus"
a[which(a[,alength]!='c15'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="esophagus"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c15'),bxlength]<-0
ax[which(ax[,axlength]!='c15'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_esophagus_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 3, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 3, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 3, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 3, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_esophagus <- svm(x_dt_all,y_dt_all, gamma = 0.001, cost=100)
save(model_dt_all_esophagus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_big_chr.Rdata")


print(predict(model_dt_all_esophagus,test_one))
print(predict(model_dt_all_esophagus,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_esophagus <- svm(x_dt_samll,y_dt_samll, gamma = 0.001, cost=100)
save(model_dt_samll_esophagus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_samll_chr.Rdata")


print(predict(model_dt_samll_esophagus,test_one))
print(predict(model_dt_samll_esophagus,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_esophagus <- svm(x_dxt_all,y_dxt_all, gamma = 0.001, cost=100)
save(model_dxt_all_esophagus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_big_num.Rdata")


print(predict(model_dxt_all_esophagus,test_one))
print(predict(model_dxt_all_esophagus,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_esophagus <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.001, cost=100)
save(model_dxt_samll_esophagus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_samll_num.Rdata")

print(predict(model_dxt_samll_esophagus,test_one))
print(predict(model_dxt_samll_esophagus,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_esophagus_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  100, mtry  =  30)
save(model_dt_all_esophagus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_big_chr.Rdata")

print(predict(model_dt_all_esophagus_rf,test_one))
print(predict(model_dt_all_esophagus_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_esophagus_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  100, mtry  =  30)
save(model_dt_samll_esophagus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_samll_chr.Rdata")

print(predict(model_dt_samll_esophagus_rf,test_one))
print(predict(model_dt_samll_esophagus_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_esophagus_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  100, mtry  =  30)
save(model_dxt_all_esophagus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_esophagus_rf,test_one))
print(predict(model_dxt_all_esophagus_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_esophagus_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  100, mtry  =  30)
save(model_dxt_samll_esophagus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_samll_num.Rdata")

print(predict(model_dxt_samll_esophagus_rf,test_one))
print(predict(model_dxt_samll_esophagus_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-7)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(kidney){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"kidney"}else{y<-"other"})
#dt<-a

###kidney_kidney
blength<-length(a[1,])
a[which(a[,alength]=='c64'),blength]<-"kidney"
a[which(a[,alength]!='c64'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="kidney"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c64'),bxlength]<-0
ax[which(ax[,axlength]!='c64'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_kidney_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 3, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 3, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 3, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 3, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_kidney <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_kidney,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_big_chr.Rdata")


print(predict(model_dt_all_kidney,test_one))
print(predict(model_dt_all_kidney,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_kidney <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_kidney,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_samll_chr.Rdata")


print(predict(model_dt_samll_kidney,test_one))
print(predict(model_dt_samll_kidney,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_kidney <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_kidney,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_big_num.Rdata")


print(predict(model_dxt_all_kidney,test_one))
print(predict(model_dxt_all_kidney,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_kidney <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_kidney,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_samll_num.Rdata")

print(predict(model_dxt_samll_kidney,test_one))
print(predict(model_dxt_samll_kidney,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_kidney_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_kidney_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_big_chr.Rdata")

print(predict(model_dt_all_kidney_rf,test_one))
print(predict(model_dt_all_kidney_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_kidney_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_kidney_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_samll_chr.Rdata")

print(predict(model_dt_samll_kidney_rf,test_one))
print(predict(model_dt_samll_kidney_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_kidney_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_kidney_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_kidney_rf,test_one))
print(predict(model_dxt_all_kidney_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_kidney_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_kidney_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_samll_num.Rdata")

print(predict(model_dxt_samll_kidney_rf,test_one))
print(predict(model_dxt_samll_kidney_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-8)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(liver){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"liver"}else{y<-"other"})
#dt<-a

###liver_liver
blength<-length(a[1,])
a[which(a[,alength]=='c22'),blength]<-"liver"
a[which(a[,alength]!='c22'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="liver"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c22'),bxlength]<-0
ax[which(ax[,axlength]!='c22'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_liver_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 3, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 3, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 3, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 3, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_liver <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=1)
save(model_dt_all_liver,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_big_chr.Rdata")


print(predict(model_dt_all_liver,test_one))
print(predict(model_dt_all_liver,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_liver <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=1)
save(model_dt_samll_liver,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_samll_chr.Rdata")


print(predict(model_dt_samll_liver,test_one))
print(predict(model_dt_samll_liver,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_liver <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=1)
save(model_dxt_all_liver,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_big_num.Rdata")


print(predict(model_dxt_all_liver,test_one))
print(predict(model_dxt_all_liver,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_liver <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=1)
save(model_dxt_samll_liver,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_samll_num.Rdata")

print(predict(model_dxt_samll_liver,test_one))
print(predict(model_dxt_samll_liver,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_liver_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_liver_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_big_chr.Rdata")

print(predict(model_dt_all_liver_rf,test_one))
print(predict(model_dt_all_liver_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_liver_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_liver_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_samll_chr.Rdata")

print(predict(model_dt_samll_liver_rf,test_one))
print(predict(model_dt_samll_liver_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_liver_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_liver_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_liver_rf,test_one))
print(predict(model_dxt_all_liver_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_liver_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_liver_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_samll_num.Rdata")

print(predict(model_dxt_samll_liver_rf,test_one))
print(predict(model_dxt_samll_liver_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-9)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(pancreas){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"pancreas"}else{y<-"other"})
#dt<-a

###pancreas_pancreas
blength<-length(a[1,])
a[which(a[,alength]=='c25'),blength]<-"pancreas"
a[which(a[,alength]!='c25'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="pancreas"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c25'),bxlength]<-0
ax[which(ax[,axlength]!='c25'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_pancreas_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 5, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 5, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 5, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 5, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_pancreas <- svm(x_dt_all,y_dt_all, gamma = 0.001, cost=100)
save(model_dt_all_pancreas,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_big_chr.Rdata")


print(predict(model_dt_all_pancreas,test_one))
print(predict(model_dt_all_pancreas,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_pancreas <- svm(x_dt_samll,y_dt_samll, gamma = 0.001, cost=100)
save(model_dt_samll_pancreas,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_samll_chr.Rdata")


print(predict(model_dt_samll_pancreas,test_one))
print(predict(model_dt_samll_pancreas,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_pancreas <- svm(x_dxt_all,y_dxt_all, gamma = 0.001, cost=100)
save(model_dxt_all_pancreas,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_big_num.Rdata")


print(predict(model_dxt_all_pancreas,test_one))
print(predict(model_dxt_all_pancreas,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_pancreas <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.001, cost=100)
save(model_dxt_samll_pancreas,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_samll_num.Rdata")

print(predict(model_dxt_samll_pancreas,test_one))
print(predict(model_dxt_samll_pancreas,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_pancreas_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_pancreas_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_big_chr.Rdata")

print(predict(model_dt_all_pancreas_rf,test_one))
print(predict(model_dt_all_pancreas_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_pancreas_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_pancreas_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_samll_chr.Rdata")

print(predict(model_dt_samll_pancreas_rf,test_one))
print(predict(model_dt_samll_pancreas_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_pancreas_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_pancreas_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_pancreas_rf,test_one))
print(predict(model_dxt_all_pancreas_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_pancreas_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_pancreas_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_samll_num.Rdata")

print(predict(model_dxt_samll_pancreas_rf,test_one))
print(predict(model_dxt_samll_pancreas_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-10)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(prostate){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"prostate"}else{y<-"other"})
#dt<-a

###prostate_prostate
blength<-length(a[1,])
a[which(a[,alength]=='c61'),blength]<-"prostate"
a[which(a[,alength]!='c61'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="prostate"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c61'),bxlength]<-0
ax[which(ax[,axlength]!='c61'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_prostate_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 5, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 5, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 5, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 5, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_prostate <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_prostate,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_big_chr.Rdata")


print(predict(model_dt_all_prostate,test_one))
print(predict(model_dt_all_prostate,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_prostate <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_prostate,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_samll_chr.Rdata")


print(predict(model_dt_samll_prostate,test_one))
print(predict(model_dt_samll_prostate,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_prostate <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_prostate,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_big_num.Rdata")


print(predict(model_dxt_all_prostate,test_one))
print(predict(model_dxt_all_prostate,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_prostate <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_prostate,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_samll_num.Rdata")

print(predict(model_dxt_samll_prostate,test_one))
print(predict(model_dxt_samll_prostate,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_prostate_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_prostate_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_big_chr.Rdata")

print(predict(model_dt_all_prostate_rf,test_one))
print(predict(model_dt_all_prostate_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_prostate_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_prostate_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_samll_chr.Rdata")

print(predict(model_dt_samll_prostate_rf,test_one))
print(predict(model_dt_samll_prostate_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_prostate_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_prostate_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_prostate_rf,test_one))
print(predict(model_dxt_all_prostate_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_prostate_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_prostate_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_samll_num.Rdata")

print(predict(model_dxt_samll_prostate_rf,test_one))
print(predict(model_dxt_samll_prostate_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}


print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-11)
print(123456789)
print(123456789)
print(123456789)
print(123456789)


if(stomach){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"stomach"}else{y<-"other"})
#dt<-a

###stomach_stomach
blength<-length(a[1,])
a[which(a[,alength]=='c16'),blength]<-"stomach"
a[which(a[,alength]!='c16'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="stomach"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c16'),bxlength]<-0
ax[which(ax[,axlength]!='c16'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_stomach_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 3, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 3, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 3, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 3, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_stomach <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_stomach,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_big_chr.Rdata")


print(predict(model_dt_all_stomach,test_one))
print(predict(model_dt_all_stomach,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_stomach <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_stomach,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_samll_chr.Rdata")


print(predict(model_dt_samll_stomach,test_one))
print(predict(model_dt_samll_stomach,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_stomach <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_stomach,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_big_num.Rdata")


print(predict(model_dxt_all_stomach,test_one))
print(predict(model_dxt_all_stomach,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_stomach <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_stomach,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_samll_num.Rdata")

print(predict(model_dxt_samll_stomach,test_one))
print(predict(model_dxt_samll_stomach,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_stomach_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  30)
save(model_dt_all_stomach_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_big_chr.Rdata")

print(predict(model_dt_all_stomach_rf,test_one))
print(predict(model_dt_all_stomach_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_stomach_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  30)
save(model_dt_samll_stomach_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_samll_chr.Rdata")

print(predict(model_dt_samll_stomach_rf,test_one))
print(predict(model_dt_samll_stomach_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_stomach_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  30)
save(model_dxt_all_stomach_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_stomach_rf,test_one))
print(predict(model_dxt_all_stomach_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_stomach_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  30)
save(model_dxt_samll_stomach_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_samll_num.Rdata")

print(predict(model_dxt_samll_stomach_rf,test_one))
print(predict(model_dxt_samll_stomach_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-12)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(thyrold){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"thyrold"}else{y<-"other"})
#dt<-a

###thyrold_thyrold
blength<-length(a[1,])
a[which(a[,alength]=='c73'),blength]<-"thyrold"
a[which(a[,alength]!='c73'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="thyrold"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c73'),bxlength]<-0
ax[which(ax[,axlength]!='c73'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_thyrold_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 5, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 5, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 5, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 5, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_thyrold <- svm(x_dt_all,y_dt_all, gamma = 0.0001, cost=100)
save(model_dt_all_thyrold,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_big_chr.Rdata")


print(predict(model_dt_all_thyrold,test_one))
print(predict(model_dt_all_thyrold,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_thyrold <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_thyrold,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_samll_chr.Rdata")


print(predict(model_dt_samll_thyrold,test_one))
print(predict(model_dt_samll_thyrold,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_thyrold <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_thyrold,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_big_num.Rdata")


print(predict(model_dxt_all_thyrold,test_one))
print(predict(model_dxt_all_thyrold,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_thyrold <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_thyrold,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_samll_num.Rdata")

print(predict(model_dxt_samll_thyrold,test_one))
print(predict(model_dxt_samll_thyrold,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_thyrold_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_thyrold_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_big_chr.Rdata")

print(predict(model_dt_all_thyrold_rf,test_one))
print(predict(model_dt_all_thyrold_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_thyrold_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_thyrold_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_samll_chr.Rdata")

print(predict(model_dt_samll_thyrold_rf,test_one))
print(predict(model_dt_samll_thyrold_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_thyrold_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_thyrold_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_thyrold_rf,test_one))
print(predict(model_dxt_all_thyrold_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_thyrold_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_thyrold_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_samll_num.Rdata")

print(predict(model_dxt_samll_thyrold_rf,test_one))
print(predict(model_dxt_samll_thyrold_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-13)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(uterus){
a<-a_zp
rownames(a)<-a[,1]
a<-a[,-1]
alength<-length(a[1,])
arowlength<-length(a[,1])
a$para<-c(1:arowlength)
#a$para<-sapply(a[,alength],a$para,function(x,y) if(x=="c18"|x=="c19"|x=="c20"){y<-"uterus"}else{y<-"other"})
#dt<-a

###uterus_uterus
blength<-length(a[1,])
a[which(a[,alength]=='c54'),blength]<-"uterus"
a[which(a[,alength]!='c54'),blength]<-"other"

a<-a[,-alength]
a<-as.data.frame(a)
a<-na.omit(a)


######################构建第一个7500数据集合模型##########################

dt_all <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

a1<-a[which(a[,alength]=="uterus"),]
a2<-a[which(a[,alength]=="other"),]
a1_length<-length(a1[,1])
a2_length<-length(a2[,1])
a1_length<-a1_length*2
a3_list<-round(runif(a1_length,0,a2_length))
a2<-a2[a3_list,]

a<-rbind(a1,a2)
a<-na.omit(a)


dt_samll <- data.frame(a[,-length(a[1,])],Mut=a[,length(a[1,])])

############################构建数据0-1####################################

ax<-a_zp
rownames(ax)<-ax[,1]
ax<-ax[,-1]
axlength<-length(ax[1,])
axrowlength<-length(ax[,1])
ax$para<-c(1:axrowlength)
bxlength<-length(ax[1,])
ax[which(ax[,axlength]=='c54'),bxlength]<-0
ax[which(ax[,axlength]!='c54'),bxlength]<-1

ax<-ax[,-axlength]
ax<-as.data.frame(ax)

test_one <- ax[5000,-length(ax[1,])]

test_len<-length(ax[1,])
print(test_len)
#print(ax[5000,length(ax[1,])])
print("选择结果5000")


###########################构建全部数据模型

ax<-na.omit(ax)
dxt_all <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

ax1<-ax[which(ax[,axlength]==0),]
ax2<-ax[which(ax[,axlength]==1),]
ax1_length<-length(ax1[,1])
ax2_length<-length(ax2[,1])
ax1_length<-ax1_length*2
ax3_list<-round(runif(ax1_length,0,ax2_length))
ax2<-ax2[ax3_list,]

ax<-rbind(ax1,ax2)
ax<-na.omit(ax)

dxt_samll <- data.frame(ax[,-length(ax[1,])],Mut=ax[,length(ax[1,])])

test_two <- ax[20,-length(ax[1,])]

#print(ax[20,length(ax[1,])])
print("选择结果20")

######################knn测试#############################

######################knn_uterus_max_char######################

train_dt_all<- dt_all[,-length(dt_all[1,])]
lab_dt_all<-dt_all[,length(dt_all[1,])]
test_two
test_one

save(train_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_big.Rdata")
save(lab_dt_all,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_big_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_big.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_big_lab.Rdata")

print("first_big_answer")
print(knn(train_dt_all, test_one, cl=lab_dt_all, k = 11, prob=TRUE))
print(knn(train_dt_all, test_two, cl=lab_dt_all, k = 11, prob=TRUE))


train_dt_samll<- dt_samll[,-length(dt_samll[1,])]
lab_dt_samll<-dt_samll[,length(dt_samll[1,])]

save(train_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_samll.Rdata")
save(lab_dt_samll,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_samll_lab.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_samll.Rdata")
#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_samll_lab.Rdata")

print("second_samll_answer")
print(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 11, prob=TRUE))
print(knn(train_dt_samll, test_two, cl=lab_dt_samll, k = 11, prob=TRUE))






############svm_test###################


print("svm_test")

############svm_all_chart##############


x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_uterus <- svm(x_dt_all,y_dt_all, gamma = 0.001, cost=1)
save(model_dt_all_uterus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_big_chr.Rdata")


print(predict(model_dt_all_uterus,test_one))
print(predict(model_dt_all_uterus,test_two))



############svm_samll_chart#############


x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_uterus <- svm(x_dt_samll,y_dt_samll, gamma = 0.0001, cost=100)
save(model_dt_samll_uterus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_samll_chr.Rdata")


print(predict(model_dt_samll_uterus,test_one))
print(predict(model_dt_samll_uterus,test_two))



############svm_all_0_1#####################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_uterus <- svm(x_dxt_all,y_dxt_all, gamma = 0.0001, cost=100)
save(model_dxt_all_uterus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_big_num.Rdata")


print(predict(model_dxt_all_uterus,test_one))
print(predict(model_dxt_all_uterus,test_two))





############svm_samll_0_1###################

x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_uterus <- svm(x_dxt_samll,y_dxt_samll, gamma = 0.0001, cost=100)
save(model_dxt_samll_uterus,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_samll_num.Rdata")


#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_samll_num.Rdata")

print(predict(model_dxt_samll_uterus,test_one))
print(predict(model_dxt_samll_uterus,test_two))




############randomForest_test###################

print("randomforest")
print("aaaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbbb")
print("cccccccccccccccccccccccccc")


############randomForest_big###################

x_dt_all <- subset(dt_all, select = -Mut)
y_dt_all <- dt_all$Mut
model_dt_all_uterus_rf<-randomForest(x_dt_all,y_dt_all, ntree  =  500, mtry  =  5)
save(model_dt_all_uterus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_big_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_big_chr.Rdata")

print(predict(model_dt_all_uterus_rf,test_one))
print(predict(model_dt_all_uterus_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")


###########randomforest_samll####################

x_dt_samll <- subset(dt_samll, select = -Mut)
y_dt_samll <- dt_samll$Mut
model_dt_samll_uterus_rf<-randomForest(x_dt_samll,y_dt_samll, ntree  =  500, mtry  =  5)
save(model_dt_samll_uterus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_samll_chr.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_samll_chr.Rdata")

print(predict(model_dt_samll_uterus_rf,test_one))
print(predict(model_dt_samll_uterus_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")







#############randomForest_big_unm##################


x_dxt_all <- subset(dxt_all, select = -Mut)
y_dxt_all <- dxt_all$Mut
model_dxt_all_uterus_rf<-randomForest(x_dxt_all,y_dxt_all, ntree  =  500, mtry  =  5)
save(model_dxt_all_uterus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_big_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_big_num.Rdata")

print("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

print(predict(model_dxt_all_uterus_rf,test_one))
print(predict(model_dxt_all_uterus_rf,test_two))



#############randomfores_samll_num#####################


x_dxt_samll <- subset(dxt_samll, select = -Mut)
y_dxt_samll <- dxt_samll$Mut
model_dxt_samll_uterus_rf<-randomForest(x_dxt_samll,y_dxt_samll, ntree  =  500, mtry  =  5)
save(model_dxt_samll_uterus_rf,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_samll_num.Rdata")

#load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_samll_num.Rdata")

print(predict(model_dxt_samll_uterus_rf,test_one))
print(predict(model_dxt_samll_uterus_rf,test_two))

print("aaaaaaaaaaaaaaaaaaaaaaaaa")
print("bbbbbbbbbbbbbbbbbbbbbbbbb")



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(123456789-14)
print(123456789)
print(123456789)
print(123456789)
print(123456789)