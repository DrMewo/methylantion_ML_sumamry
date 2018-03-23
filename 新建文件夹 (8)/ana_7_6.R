#z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")
#z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))
print(1)
library(mclust)
print(2)
for(k in c(1:30)){
para_name_summary<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/6/data_summary",k,sep="_")
para_name_summary<-paste(para_name_summary,".txt",sep="")
A<-read.delim(para_name_summary)
B<-t(A)
C<-B[2:length(B[,1]),1:42]
D<-apply(C,2,as.numeric)
print(head(D))
Cnames<-A[1:42,1]
Rnames<-rownames(C)
print(100000)
print(Cnames)
print(Rnames)
print(length(Cnames))
print(length(D[1,]))
print(length(Rnames))
print(length(D[,1]))
colnames(D)<-Cnames
rownames(D)<-Rnames
E<-na.omit(D)
print("abcabc")
a<-Mclust(as.matrix(E),G=1:25)
b<-data.frame(a$classification)
b$name<-rownames(b)
colnames(b)<-c("class","names")
print("bcdbcd")
k1<-apply(E,1,min)
h<-data.frame(k1)
h$name<-rownames(h)
colnames(h)<-c("pvalue","names")
data_ak<-merge(b,h,x.by="names",y.by="names")

data_ak1<-data.frame(tapply(data_ak$pvalue,data_ak$class,min))
colnames(data_ak1)<-c("pvalue")

data_ak2<-merge(data_ak,data_ak1,x.by="pvalue",y.by="pvalue")

para_name_summary_1<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/6/data_kmeans",k,sep="_")
para_name_summary_1<-paste(para_name_summary_1,".txt",sep="")
write.table(data_ak,para_name_summary_1,sep="\t",row.names = FALSE)

para_name_summary_2<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/6/data_kmeans_sum",k,sep="_")
para_name_summary_2<-paste(para_name_summary_2,".txt",sep="")
write.table(data_ak2,para_name_summary_2,sep="\t",row.names = FALSE)
}


