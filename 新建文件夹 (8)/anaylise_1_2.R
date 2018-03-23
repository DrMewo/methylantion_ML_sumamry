z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")

z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))

n<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_10_2_6.txt")
m<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_10_2_4.txt")
a<-merge(m,n,x.by="ref",y.by="ref")
write.table(a,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_10_2_ana.txt",sep="\t",row.names=F)

a[is.na(a)] <- 0

a_matrix<-matrix(,nrow=nrow(a),ncol=ncol(a)-1)

for(i in c(1:nrow(a))){
for(j in c(1:ncol(a)-1)){
a_matrix[i,j]<-a[i,j+1]
}
}

g<-colnames(a)
g<-g[-1]
k<-c(1:length(g))

for(i in c(1:length(g))){
k[i]<-as.character(z[which(as.character(z[,15])==g[i]),4])
}


pvalue<- data.frame(a[,1],c(1:length(a[,1])),c(1:length(a[,1])))
for(i in c(1:length(a_matrix[,1]))){
a<-a_matrix[i,]
b<-k
c<-data.frame(a,b)
pvalue[i,2]<-bartlett.test(a~b,data=c)$p.value
pvalue[i,3]<-sum(a)/length(a)
}

write.table(pvalue,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/analysis_1_2_10_F.txt",sep="\t",row.names=F)
write.table(pvalue,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/analysis_1_2_10_T.txt",sep="\t")

