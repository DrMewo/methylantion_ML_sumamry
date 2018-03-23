
library(Biobase)
library(ALL)
library(genefilter)
library(DMwR)
library(class)
library(lattice)
library(Hmisc)
library(randomForest)
library(e1071)

#dat = load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/R/GSE38240.Rd")
#dat = load("/mnt/ilustre/users/sanger-dev/sg-users/shenyiru/ctDNA/GEO_DB/GSE59157.Rd")

#path_param<-"/mnt/ilustre/users/sanger-dev/sg-users/shenyiru/ctDNA/GEO_DB/GSE59157.Rd"

#path_param<-"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/R/GSE37816.Rd"

hope<-function(path_param){

dat = load(path_param)

print("open_sucess")

ffff<-unlist(strsplit(path_param,split="/"))
ffff_length<-length(ffff)
ffff_name<-ffff[ffff_length]
ffff_out_name<-unlist(strsplit(ffff_name,split="[.]"))[1]


if (grepl("data", dat[1])){
    data = eval(as.symbol(dat[1]))
    info = eval(as.symbol(dat[2]))
}else {
    data = eval(as.symbol(dat[2]))
    info = eval(as.symbol(dat[1]))
}

#data = read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/R/first_test.txt")

data_colnames<-colnames(data)
data_rownames<-rownames(data)

data_length_one<-length(data[1,])

for(s in c(1:data_length_one)){



a<-data.frame(as.numeric(as.character(data[,s])))
colnames(a)<-data_colnames[s]
rownames(a)<-data_rownames

message_names<-colnames(a)

a_1<-data.frame(rownames(a),a)
a_colnames<-colnames(a_1)
colnames(a_1)<-c("names",a_colnames[2])


f<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_data/all_class.txt")


k<-merge(f,a_1,x.ref="names",y.ref="names")

k<-as.data.frame(k)

k_colnames<-colnames(k)
colnames(k)<-c("names","class","means")


k_len_list<-c(1:length(k[,1]))


k<-k[,-1]


l<-tapply(k$means,k$class,mean)




h_1<-data.frame(names(l),l)

colnames(h_1)<-c("class","beat_value_means")

h_1<-na.omit(h_1)


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/test_data/test_2.Rdata")

h_2<-t(test_two)
h_3<-data.frame(rownames(h_2),h_2)
colnames(h_3)<-c("names","beta_value")



n_1<-merge(h_3,a_1,x.by="names",y.by="names")
n_2<-n_1[,-2]

colnames(n_2)<-c("names","num")
n_3<-merge(n_2,h_3,all=TRUE)

n_4<-n_3[,-3]


n_4[is.na(n_4[,2]),2]=-1


rownames(n_4)<-n_4[,1]



find_site<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_data/imp_class.txt")


find_site<-find_site[!duplicated(find_site$names),]



m_1<-merge(n_4,find_site,all=TRUE)

all_ava<-mean(h_1[,2])

for(i in c(1:length(m_1[,1]))){
    if(m_1[i,2]==-1){
        if(sum(which(m_1[i,3]==h_1[,1]))==0){
            m_1[i,2]<-all_ava
        }else{
            m_1[i,2]<-h_1[which(m_1[i,3]==h_1[,1]),2]
        }
    }
}

out_put_in_data<-data.frame(m_1[,2])
colnames(out_put_in_data)<-colnames(m_1)[2]
rownames(out_put_in_data)<-m_1[,1]

out_put_in_data_1<-t(out_put_in_data)

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/test_data/test_2.Rdata")

h_2<-t(test_two)
h_3<-data.frame(rownames(h_2),h_2)
colnames(h_3)<-c("names","beta_value")



n_1<-merge(h_3,a_1,x.by="names",y.by="names")
n_2<-n_1[,-2]

colnames(n_2)<-c("names","num")
n_3<-merge(n_2,h_3,all=TRUE)

n_4<-n_3[,-3]


n_4[is.na(n_4[,2]),2]=-1


rownames(n_4)<-n_4[,1]



find_site<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_data/imp_class.txt")


find_site<-find_site[!duplicated(find_site$names),]



m_1<-merge(n_4,find_site,all=TRUE)

all_ava<-mean(h_1[,2])

for(i in c(1:length(m_1[,1]))){
    if(m_1[i,2]==-1){
        if(sum(which(m_1[i,3]==h_1[,1]))==0){
            m_1[i,2]<-all_ava
        }else{
            m_1[i,2]<-h_1[which(m_1[i,3]==h_1[,1]),2]
        }
    }
}

out_put_in_data<-data.frame(m_1[,2])
colnames(out_put_in_data)<-colnames(m_1)[2]
rownames(out_put_in_data)<-m_1[,1]

#print(out_put_in_data)

out_put_in_data_1<-t(out_put_in_data)

test_one<-out_put_in_data_1
test_two<-out_put_in_data_1


fuck=2
if(fuck==2){
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
}

if(fuck==1){
adrenal_gland<-TRUE
colorectal<-FALSE
lung<-FALSE
bladder<-FALSE
breast<-FALSE
cervix<-FALSE
esophagus<-FALSE
kidney<-FALSE
liver<-FALSE
pancreas<-FALSE
prostate<-FALSE
stomach<-FALSE
thyrold<-FALSE
uterus<-FALSE
}

name_list<-c("test_name","adrenal_gland_knn","adrenal_gland_svm","adrenal_gland_randomforest","colorectal_knn","colorectal_svm","colorectal_randomforest","lung_knn","lung_svm","lung_randomforest","bladder_knn","bladder_svm","bladder_randomforest","breast_knn","breast_svm","breast_randomforest","cervix_knn","cervix_svm","cervix_randomforest","esophagus_knn","esophagus_svm","esophagus_randomforest","kidney_knn","kidney_svm","kidney_randomforest","liver_knn","liver_svm","liver_randomforest","pancreas_knn","pancreas_svm","pancreas_randomforest","prostate_knn","prostate_svm","prostate_randomforest","stomach_knn","stomach_svm","stomach_randomforest","thyrold_knn","thyrold_svm","thyrold_randomforest","uterus_knn","uterus_svm","uterus_randomforest")
num_list<-c(1:43)

out_put_chr_all<-data.frame(name_list,num_list)
out_put_chr_samll<-data.frame(name_list,num_list)
out_put_num_all<-data.frame(name_list,num_list)
out_put_num_samll<-data.frame(name_list,num_list)
colnames(out_put_chr_all)<-c("para_list","ans")
colnames(out_put_chr_samll)<-c("para_list","ans")
colnames(out_put_num_all)<-c("para_list","ans")
colnames(out_put_num_samll)<-c("para_list","ans")

out_put_chr_all[1,2]<-message_names
out_put_chr_samll[1,2]<-message_names
out_put_num_all[1,2]<-message_names
out_put_num_samll[1,2]<-message_names

if(adrenal_gland){

print("选择结果")

######################knn测试#############################

######################knn_adrenal_gland_max_char######################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_big_lab.Rdata")



out_put_chr_all[2,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[2,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[2,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/knn_adrenal_gland_samll_lab.Rdata")



out_put_chr_samll[2,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[2,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[2,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}



############svm_test###################

print("svm_test")

############svm_all_chart##############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_big_chr.Rdata")


model_one<-model_dt_all_adrenal_gland
out_put_chr_all[3,2]<-as.character(predict(model_one,test_one)[1])

############svm_samll_chart#############

print("svm_test_2")

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_samll_chr.Rdata")


model_one<-model_dt_samll_adrenal_gland
out_put_chr_samll[3,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################

print("svm_test_3")

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_big_num.Rdata")



model_one<-model_dxt_all_adrenal_gland
out_put_num_all[3,2]<-1-as.numeric(predict(model_one,test_one)[1])

############svm_samll_0_1###################

print("svm_test_3")

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/svm_adrenal_gland_samll_num.Rdata")


model_one<-model_dxt_samll_adrenal_gland
out_put_num_samll[3,2]<-1-as.numeric(predict(model_one,test_one)[1])





############randomForest_test###################

print("randomforest_1")



############randomForest_big###################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_big_chr.Rdata")

model_two<-model_dt_all_adrenal_gland_rf
out_put_chr_all[4,2]<-as.character(predict(model_two,test_one)[1])

###########randomforest_samll####################

print("randomforest_2")

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_samll_chr.Rdata")

model_two<-model_dt_samll_adrenal_gland_rf
out_put_chr_samll[4,2]<-as.character(predict(model_two,test_one)[1])

#############randomForest_big_unm##################

print("randomforest_3")


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_big_num.Rdata")

model_two<-model_dxt_all_adrenal_gland_rf
out_put_num_all[4,2]<-1-as.numeric(predict(model_two,test_one)[1])


print("randomforest_4")

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/adrenal_gland/randomforest_adrenal_gland_samll_num.Rdata")


model_two<-model_dxt_samll_adrenal_gland_rf
out_put_num_samll[4,2]<-1-as.numeric(predict(model_two,test_one)[1])

}


print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(1)
print(123456789)
print(123456789)
print(123456789)
print(123456789)


if(colorectal){

######################knn_colorectal_max_char######################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_big_lab.Rdata")

out_put_chr_all[5,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[5,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[5,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}




load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/knn_colorectal_samll_lab.Rdata")

out_put_chr_samll[5,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[5,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[5,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}





############svm_test###################


print("svm_test")

############svm_all_chart##############



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_big_chr.Rdata")

model_one<-model_dt_all_colorectal
out_put_chr_all[6,2]<-as.character(predict(model_one,test_one)[1])



############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_samll_chr.Rdata")

model_one<-model_dt_samll_colorectal
out_put_chr_samll[6,2]<-as.character(predict(model_one,test_one)[1])



############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_big_num.Rdata")

model_one<-model_dxt_all_colorectal
out_put_num_all[6,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/svm_colorectal_samll_num.Rdata")


model_one<-model_dxt_samll_colorectal
out_put_num_samll[6,2]<-1-as.numeric(predict(model_one,test_one)[1])



############randomForest_test###################


############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_big_chr.Rdata")

model_two<-model_dt_all_colorectal_rf
out_put_chr_all[7,2]<-as.character(predict(model_two,test_one)[1])

###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_samll_chr.Rdata")

model_two<-model_dt_samll_colorectal_rf
out_put_chr_samll[7,2]<-as.character(predict(model_two,test_one)[1])







#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_big_num.Rdata")

model_two<-model_dxt_all_colorectal_rf
out_put_num_all[7,2]<-1-as.numeric(predict(model_two,test_one)[1])


#############randomfores_samll_num#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/colorectal/randomforest_colorectal_samll_num.Rdata")

model_two<-model_dxt_samll_colorectal_rf
out_put_num_samll[7,2]<-1-as.numeric(predict(model_two,test_one)[1])



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(2)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(lung){

######################knn测试#############################

######################knn_lung_max_char######################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_big_lab.Rdata")

out_put_chr_all[8,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[8,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[8,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/knn_lung_samll_lab.Rdata")

out_put_chr_samll[8,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[8,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[8,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_big_chr.Rdata")

model_one<-model_dt_all_lung
out_put_chr_all[9,2]<-as.character(predict(model_one,test_one)[1])


############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_samll_chr.Rdata")


model_one<-model_dt_samll_lung
out_put_chr_samll[9,2]<-as.character(predict(model_one,test_one)[1])


############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_big_num.Rdata")


model_one<-model_dxt_all_lung
out_put_num_all[9,2]<-1-as.numeric(predict(model_one,test_one)[1])

############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/svm_lung_samll_num.Rdata")


model_one<-model_dxt_samll_lung
out_put_num_samll[9,2]<-1-as.numeric(predict(model_one,test_one)[1])



############randomForest_test###################




############randomForest_big###################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_big_chr.Rdata")

model_two<-model_dt_all_lung_rf
out_put_chr_all[10,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_samll_chr.Rdata")

model_two<-model_dt_samll_lung_rf
out_put_chr_samll[10,2]<-as.character(predict(model_two,test_one)[1])

#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_big_num.Rdata")

model_two<-model_dxt_all_lung_rf
out_put_num_all[10,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/lung/randomforest_lung_samll_num.Rdata")

model_two<-model_dxt_samll_lung_rf
out_put_num_samll[10,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(3)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(bladder){


######################knn测试#############################

######################knn_bladder_max_char######################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_big_lab.Rdata")

out_put_chr_all[11,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[11,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[11,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/knn_bladder_samll_lab.Rdata")

out_put_chr_samll[11,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[11,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[11,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_big_chr.Rdata")

model_one<-model_dt_all_bladder
out_put_chr_all[12,2]<-as.character(predict(model_one,test_one)[1])



############svm_samll_chart#############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_samll_chr.Rdata")


model_one<-model_dt_samll_bladder
out_put_chr_samll[12,2]<-as.character(predict(model_one,test_one)[1])


############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_big_num.Rdata")


model_one<-model_dxt_all_bladder
out_put_num_all[12,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/svm_bladder_samll_num.Rdata")


model_one<-model_dxt_samll_bladder
out_put_num_samll[12,2]<-1-as.numeric(predict(model_one,test_one)[1])



############randomForest_test###################




############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_big_chr.Rdata")

model_two<-model_dt_all_bladder_rf
out_put_chr_all[13,2]<-as.character(predict(model_two,test_one)[1])

###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_samll_chr.Rdata")

model_two<-model_dt_samll_bladder_rf
out_put_chr_samll[13,2]<-as.character(predict(model_two,test_one)[1])

#############randomForest_big_unm##################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_big_num.Rdata")

model_two<-model_dxt_all_bladder_rf
out_put_num_all[13,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/bladder/randomforest_bladder_samll_num.Rdata")

model_two<-model_dxt_samll_bladder_rf
out_put_num_samll[13,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(4)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(breast){


######################knn测试#############################

######################knn_breast_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_big_lab.Rdata")

out_put_chr_all[14,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[14,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[14,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}




load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/knn_breast_samll_lab.Rdata")

out_put_chr_samll[14,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[14,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[14,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_big_chr.Rdata")


model_one<-model_dt_all_breast
out_put_chr_all[15,2]<-as.character(predict(model_one,test_one)[1])



############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_samll_chr.Rdata")

model_one<-model_dt_samll_breast
out_put_chr_samll[15,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################




load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_big_num.Rdata")

model_one<-model_dxt_all_breast
out_put_num_all[15,2]<-1-as.numeric(predict(model_one,test_one)[1])

############svm_samll_0_1###################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/svm_breast_samll_num.Rdata")


model_one<-model_dxt_samll_breast
out_put_num_samll[15,2]<-1-as.numeric(predict(model_one,test_one)[1])




############randomForest_test###################




############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_big_chr.Rdata")

model_two<-model_dt_all_breast_rf
out_put_chr_all[16,2]<-as.character(predict(model_two,test_one)[1])

###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_samll_chr.Rdata")

model_two<-model_dt_samll_breast_rf
out_put_chr_samll[16,2]<-as.character(predict(model_two,test_one)[1])

#############randomForest_big_unm##################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_big_num.Rdata")

model_two<-model_dxt_all_breast_rf
out_put_num_all[16,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/breast/randomforest_breast_samll_num.Rdata")

model_two<-model_dxt_samll_breast_rf
out_put_num_samll[16,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(5)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(cervix){

######################knn测试#############################

######################knn_cervix_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_big_lab.Rdata")

out_put_chr_all[17,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[17,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[17,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/knn_cervix_samll_lab.Rdata")

out_put_chr_samll[17,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[17,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[17,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_big_chr.Rdata")

model_one<-model_dt_all_cervix
out_put_chr_all[18,2]<-as.character(predict(model_one,test_one)[1])


############svm_samll_chart#############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_samll_chr.Rdata")

model_one<-model_dt_samll_cervix
out_put_chr_samll[18,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_big_num.Rdata")


model_one<-model_dxt_all_cervix
out_put_num_all[18,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/svm_cervix_samll_num.Rdata")


model_one<-model_dxt_samll_cervix
out_put_num_samll[18,2]<-1-as.numeric(predict(model_one,test_one)[1])



############randomForest_test###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_big_chr.Rdata")

model_two<-model_dt_all_cervix_rf
out_put_chr_all[19,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_samll_chr.Rdata")

model_two<-model_dt_samll_cervix_rf
out_put_chr_samll[19,2]<-as.character(predict(model_two,test_one)[1])

#############randomForest_big_unm##################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_big_num.Rdata")

model_two<-model_dxt_all_cervix_rf
out_put_num_all[19,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/cervix/randomforest_cervix_samll_num.Rdata")

model_two<-model_dxt_samll_cervix_rf
out_put_num_samll[19,2]<-1-as.numeric(predict(model_two,test_one)[1])

}


print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(6)
print(123456789)
print(123456789)
print(123456789)
print(123456789)


if(esophagus){


######################knn测试#############################

######################knn_esophagus_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_big_lab.Rdata")

out_put_chr_all[20,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[20,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[20,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/knn_esophagus_samll_lab.Rdata")

out_put_chr_samll[20,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[20,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[20,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_big_chr.Rdata")

model_one<-model_dt_all_esophagus
out_put_chr_all[21,2]<-as.character(predict(model_one,test_one)[1])


############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_samll_chr.Rdata")

model_one<-model_dt_samll_esophagus
out_put_chr_samll[21,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_big_num.Rdata")

model_one<-model_dxt_all_esophagus
out_put_num_all[21,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/svm_esophagus_samll_num.Rdata")

model_one<-model_dxt_samll_esophagus
out_put_num_samll[21,2]<-1-as.numeric(predict(model_one,test_one)[1])



############randomForest_test###################




############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_big_chr.Rdata")

model_two<-model_dt_all_esophagus_rf
out_put_chr_all[22,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_samll_chr.Rdata")

model_two<-model_dt_samll_esophagus_rf
out_put_chr_samll[22,2]<-as.character(predict(model_two,test_one)[1])


#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_big_num.Rdata")

model_two<-model_dxt_all_esophagus_rf
out_put_num_all[22,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/esophagus/randomforest_esophagus_samll_num.Rdata")

model_two<-model_dxt_samll_esophagus_rf
out_put_num_samll[22,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(7)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(kidney){

######################knn测试#############################

######################knn_kidney_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_big_lab.Rdata")

out_put_chr_all[23,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[23,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[23,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/knn_kidney_samll_lab.Rdata")

out_put_chr_samll[23,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[23,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[23,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}





############svm_test###################


print("svm_test")

############svm_all_chart##############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_big_chr.Rdata")

model_one<-model_dt_all_kidney
out_put_chr_all[24,2]<-as.character(predict(model_one,test_one)[1])

############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_samll_chr.Rdata")

model_one<-model_dt_samll_kidney
out_put_chr_samll[24,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_big_num.Rdata")


model_one<-model_dxt_all_kidney
out_put_num_all[24,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/svm_kidney_samll_num.Rdata")

model_one<-model_dxt_samll_kidney
out_put_num_samll[24,2]<-1-as.numeric(predict(model_one,test_one)[1])

############randomForest_test###################




############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_big_chr.Rdata")

model_two<-model_dt_all_kidney_rf
out_put_chr_all[25,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_samll_chr.Rdata")

model_two<-model_dt_samll_kidney_rf
out_put_chr_samll[25,2]<-as.character(predict(model_two,test_one)[1])


#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_big_num.Rdata")

model_two<-model_dxt_all_kidney_rf
out_put_num_all[25,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/kidney/randomforest_kidney_samll_num.Rdata")

model_two<-model_dxt_samll_kidney_rf
out_put_num_samll[25,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(8)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(liver){

######################knn测试#############################

######################knn_liver_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_big_lab.Rdata")

out_put_chr_all[26,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[26,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[26,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/knn_liver_samll_lab.Rdata")

out_put_chr_samll[26,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[26,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[26,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}





############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_big_chr.Rdata")

model_one<-model_dt_all_liver
out_put_chr_all[27,2]<-as.character(predict(model_one,test_one)[1])

############svm_samll_chart#############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_samll_chr.Rdata")

model_one<-model_dt_samll_liver
out_put_chr_samll[27,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_big_num.Rdata")


model_one<-model_dxt_all_liver
out_put_num_all[27,2]<-1-as.numeric(predict(model_one,test_one)[1])



############svm_samll_0_1###################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/svm_liver_samll_num.Rdata")

model_one<-model_dxt_samll_liver
out_put_num_samll[27,2]<-1-as.numeric(predict(model_one,test_one)[1])

############randomForest_test###################



############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_big_chr.Rdata")

model_two<-model_dt_all_liver_rf
out_put_chr_all[28,2]<-as.character(predict(model_two,test_one)[1])

###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_samll_chr.Rdata")

model_two<-model_dt_samll_liver_rf
out_put_chr_samll[28,2]<-as.character(predict(model_two,test_one)[1])


#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_big_num.Rdata")

model_two<-model_dxt_all_liver_rf
out_put_num_all[28,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/liver/randomforest_liver_samll_num.Rdata")

model_two<-model_dxt_samll_liver_rf
out_put_num_samll[28,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(9)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(pancreas){


######################knn测试#############################

######################knn_pancreas_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_big_lab.Rdata")

out_put_chr_all[29,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[29,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[29,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/knn_pancreas_samll_lab.Rdata")

out_put_chr_samll[29,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[29,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[29,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_big_chr.Rdata")

model_one<-model_dt_all_pancreas
out_put_chr_all[30,2]<-as.character(predict(model_one,test_one)[1])

############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_samll_chr.Rdata")

model_one<-model_dt_samll_pancreas
out_put_chr_samll[30,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_big_num.Rdata")


model_one<-model_dxt_all_pancreas
out_put_num_all[30,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/svm_pancreas_samll_num.Rdata")

model_one<-model_dxt_samll_pancreas
out_put_num_samll[30,2]<-1-as.numeric(predict(model_one,test_one)[1])

############randomForest_test###################




############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_big_chr.Rdata")

model_two<-model_dt_all_pancreas_rf
out_put_chr_all[31,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_samll_chr.Rdata")

model_two<-model_dt_samll_pancreas_rf
out_put_chr_samll[31,2]<-as.character(predict(model_two,test_one)[1])


#############randomForest_big_unm##################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_big_num.Rdata")

model_two<-model_dxt_all_pancreas_rf
out_put_num_all[31,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/pancreas/randomforest_pancreas_samll_num.Rdata")

model_two<-model_dxt_samll_pancreas_rf
out_put_num_samll[31,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(10)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(prostate){

######################knn测试#############################

######################knn_prostate_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_big_lab.Rdata")

out_put_chr_all[32,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[32,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[32,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/knn_prostate_samll_lab.Rdata")

out_put_chr_samll[32,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[32,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[32,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}





############svm_test###################


print("svm_test")

############svm_all_chart##############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_big_chr.Rdata")


model_one<-model_dt_all_prostate
out_put_chr_all[33,2]<-as.character(predict(model_one,test_one)[1])


############svm_samll_chart#############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_samll_chr.Rdata")

model_one<-model_dt_samll_prostate
out_put_chr_samll[33,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_big_num.Rdata")


model_one<-model_dxt_all_prostate
out_put_num_all[33,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/svm_prostate_samll_num.Rdata")

model_one<-model_dxt_samll_prostate
out_put_num_samll[33,2]<-1-as.numeric(predict(model_one,test_one)[1])


############randomForest_test###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_big_chr.Rdata")

model_two<-model_dt_all_prostate_rf
out_put_chr_all[34,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_samll_chr.Rdata")

model_two<-model_dt_samll_prostate_rf
out_put_chr_samll[34,2]<-as.character(predict(model_two,test_one)[1])


#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_big_num.Rdata")

model_two<-model_dxt_all_prostate_rf
out_put_num_all[34,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/prostate/randomforest_prostate_samll_num.Rdata")

model_two<-model_dxt_samll_prostate_rf
out_put_num_samll[34,2]<-1-as.numeric(predict(model_two,test_one)[1])


}


print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(11)
print(123456789)
print(123456789)
print(123456789)
print(123456789)


if(stomach){

######################knn测试#############################

######################knn_stomach_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_big_lab.Rdata")

out_put_chr_all[35,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[35,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[35,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/knn_stomach_samll_lab.Rdata")

out_put_chr_samll[35,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[35,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[35,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_big_chr.Rdata")

model_one<-model_dt_all_stomach
out_put_chr_all[36,2]<-as.character(predict(model_one,test_one)[1])

############svm_samll_chart#############

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_samll_chr.Rdata")

model_one<-model_dt_samll_stomach
out_put_chr_samll[36,2]<-as.character(predict(model_one,test_one)[1])

############svm_all_0_1#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_big_num.Rdata")

model_one<-model_dxt_all_stomach
out_put_num_all[36,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/svm_stomach_samll_num.Rdata")

model_one<-model_dxt_samll_stomach
out_put_num_samll[36,2]<-1-as.numeric(predict(model_one,test_one)[1])

############randomForest_test###################


############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_big_chr.Rdata")

model_two<-model_dt_all_stomach_rf
out_put_chr_all[37,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_samll_chr.Rdata")

model_two<-model_dt_samll_stomach_rf
out_put_chr_samll[37,2]<-as.character(predict(model_two,test_one)[1])



#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_big_num.Rdata")

model_two<-model_dxt_all_stomach_rf
out_put_num_all[37,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/stomach/randomforest_stomach_samll_num.Rdata")

model_two<-model_dxt_samll_stomach_rf
out_put_num_samll[37,2]<-1-as.numeric(predict(model_two,test_one)[1])

}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(12)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(thyrold){

######################knn测试#############################

######################knn_thyrold_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_big_lab.Rdata")

out_put_chr_all[38,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[38,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[38,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/knn_thyrold_samll_lab.Rdata")

out_put_chr_samll[38,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[38,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[38,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}






############svm_test###################


print("svm_test")

############svm_all_chart##############
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_big_chr.Rdata")

model_one<-model_dt_all_thyrold
out_put_chr_all[39,2]<-as.character(predict(model_one,test_one)[1])


############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_samll_chr.Rdata")

model_one<-model_dt_samll_thyrold
out_put_chr_samll[39,2]<-as.character(predict(model_one,test_one)[1])


############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_big_num.Rdata")


model_one<-model_dxt_all_thyrold
out_put_num_all[39,2]<-1-as.numeric(predict(model_one,test_one)[1])


############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/svm_thyrold_samll_num.Rdata")

model_one<-model_dxt_samll_thyrold
out_put_num_samll[39,2]<-1-as.numeric(predict(model_one,test_one)[1])

############randomForest_test###################



############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_big_chr.Rdata")

model_two<-model_dt_all_thyrold_rf
out_put_chr_all[40,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_samll_chr.Rdata")

model_two<-model_dt_samll_thyrold_rf
out_put_chr_samll[40,2]<-as.character(predict(model_two,test_one)[1])

#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_big_num.Rdata")

model_two<-model_dxt_all_thyrold_rf
out_put_num_all[40,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/thyrold/randomforest_thyrold_samll_num.Rdata")

model_two<-model_dxt_samll_thyrold_rf
out_put_num_samll[40,2]<-1-as.numeric(predict(model_two,test_one)[1])



}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(13)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

if(uterus){

######################knn测试#############################

######################knn_uterus_max_char######################



load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_big.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_big_lab.Rdata")

out_put_chr_all[41,2]<-as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))
if(as.character(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE))!="other"){
out_put_num_all[41,2]<-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}else{
out_put_num_all[41,2]<-1-attr(knn(train_dt_all, test_two, cl=lab_dt_all, k = 10, prob=TRUE),"prob")
}


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_samll.Rdata")
load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/knn_uterus_samll_lab.Rdata")

out_put_chr_samll[41,2]<-as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))
if(as.character(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE))!="other"){
out_put_num_samll[41,2]<-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}else{
out_put_num_samll[41,2]<-1-attr(knn(train_dt_samll, test_one, cl=lab_dt_samll, k = 10, prob=TRUE),"prob")
}





############svm_test###################


print("svm_test")

############svm_all_chart##############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_big_chr.Rdata")

model_one<-model_dt_all_uterus
out_put_chr_all[42,2]<-as.character(predict(model_one,test_one)[1])

############svm_samll_chart#############


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_samll_chr.Rdata")


model_one<-model_dt_samll_uterus
out_put_chr_samll[42,2]<-as.character(predict(model_one,test_one)[1])


############svm_all_0_1#####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_big_num.Rdata")

model_one<-model_dxt_all_uterus
out_put_num_all[42,2]<-1-as.numeric(predict(model_one,test_one)[1])



############svm_samll_0_1###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/svm_uterus_samll_num.Rdata")

model_one<-model_dxt_samll_uterus
out_put_num_samll[42,2]<-1-as.numeric(predict(model_one,test_one)[1])

############randomForest_test###################



############randomForest_big###################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_big_chr.Rdata")

model_two<-model_dt_all_uterus_rf
out_put_chr_all[43,2]<-as.character(predict(model_two,test_one)[1])


###########randomforest_samll####################

load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_samll_chr.Rdata")

model_two<-model_dt_samll_uterus_rf
out_put_chr_samll[43,2]<-as.character(predict(model_two,test_one)[1])



#############randomForest_big_unm##################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_big_num.Rdata")

model_two<-model_dxt_all_uterus_rf
out_put_num_all[43,2]<-1-as.numeric(predict(model_two,test_one)[1])

#############randomfores_samll_num#####################


load("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_model/uterus/randomforest_uterus_samll_num.Rdata")

model_two<-model_dxt_samll_uterus_rf
out_put_num_samll[43,2]<-1-as.numeric(predict(model_two,test_one)[1])


}

print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(14)
print(123456789)
print(123456789)
print(123456789)
print(123456789)





print(123456789)
print(123456789)
print(123456789)
print(123456789)
print(14)
print(123456789)
print(123456789)
print(123456789)
print(123456789)

print(out_put_chr_all)
print(out_put_chr_samll)
print(out_put_num_all)
print(out_put_num_samll)

if(s==1){

last_out_put_chr_all<-out_put_chr_all
last_out_put_chr_samll<-out_put_chr_samll
last_out_put_num_all<-out_put_num_all
last_out_put_num_samll<-out_put_num_samll

}else{

last_out_put_chr_all<-cbind(last_out_put_chr_all,out_put_chr_all[,2])
last_out_put_chr_samll<-cbind(last_out_put_chr_samll,out_put_chr_samll[,2])
last_out_put_num_all<-cbind(last_out_put_num_all,out_put_num_all[,2])
last_out_put_num_samll<-cbind(last_out_put_num_samll,out_put_num_samll[,2])


}

}



last_out_put_chr_all_path<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/out/",ffff_out_name,"_out_put_chr_all_path.txt",sep="")

last_out_put_chr_samll_path<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/out/",ffff_out_name,"_out_put_chr_one_path.txt",sep="")


last_out_put_num_all_path<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/out/",ffff_out_name,"_out_put_num_all_path.txt",sep="")


last_out_put_num_samll_path<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/out/",ffff_out_name,"_out_put_num_one_path.txt",sep="")

write.table(last_out_put_chr_all,last_out_put_chr_all_path,sep="\t")
write.table(last_out_put_chr_samll,last_out_put_chr_samll_path,sep="\t")
write.table(last_out_put_num_all,last_out_put_num_all_path,sep="\t")
write.table(last_out_put_num_samll,last_out_put_num_samll_path,sep="\t")

}

save(hope,file="/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/fun_model.Rdata")

#hope("/mnt/ilustre/users/sanger-dev/sg-users/shenyiru/ctDNA/GEO_DB/GSE34387.Rd")
