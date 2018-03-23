#z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")
#z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))
library(mclust)
for(k in c(1:30)){
para_name_summary<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/1/data_summary",k,sep="_")
para_name_summary<-paste(para_name_summary,".txt",sep="")
A<-read.delim(para_name_summary)
B<-t(A)
C<-B(2:length(B[,1]),1:42)
D<-apply(C,2,as.numeric)
Cnames<-A[1:42,1]
Rnames<-colnames(C)
colnames(D)<-Cnames
rownames(D)<-Rnames
E<-na.omit(D)
a<-Mclust(as.matrix(E),G=1:25)
b<-data.frame(a$classification)
b$name<-rownames(b)
colnames(b)<-c("class","names")

k<-apply(E,2,min)
h<-data.frame(k)
h$name<-rownames(h)
colnames(h)<-c("pvalue","names")
data_ak<-merge(b,k,x.by="names",y.by="names")

data_ak1<-data.frame(tapply(data_ak$pvalue,data_ak$class,min))
colnames(data_ak1)<-c("pvalue")

data_ak2<-merge(data_ak,data_ak1,x.by="pvalue",y.by="pvalue")

para_name_summary_1<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/1/data_kmeans",k,sep="_")
para_name_summary_1<-paste(para_name_summary_1,".txt",sep="")
write.table(data_ak,para_name_summary_1,sep="\t")

para_name_summary_2<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/1/data_kmeans_sum",k,sep="_")
para_name_summary_2<-paste(para_name_summary_1,".txt",sep="")
write.table(data_ak2,para_name_summary_2,sep="\t")
}


