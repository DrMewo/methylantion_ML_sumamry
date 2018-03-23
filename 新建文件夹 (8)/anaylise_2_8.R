z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")

z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))

a<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_10_8_ana.txt")


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

#new_list<-z[which(z[,15]==k),]

#write.table(new_list,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/new_list.txt",sep="\t")


for(i in c(1:length(g))){
k[i]<-as.character(z[which(as.character(z[,15])==g[i]),4])
}

k[which(k=="Bile_Duct")]<-"other"
k[which(k=="Lymph")]<-"other"
k[which(k=="Ovary")]<-"other"

pvalue<- data.frame(a[,1],c(1:length(a[,1])),c(1:length(a[,1])))
for(i in c(1:length(a_matrix[,1]))){
a<-a_matrix[i,]
b<-k
c<-data.frame(a,b)

pvalue[i,2]<-bartlett.test(a~b,data=c)$p.value
pvalue[i,3]<-sum(a)/length(a)
}

write.table(pvalue,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/analysis_2_8_10_F.txt",sep="\t",row.names=F)
write.table(pvalue,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/analysis_2_8_10_T.txt",sep="\t")



g<-colnames(a)
g<-g[-1]
k<-c(1:length(g))

for(i in c(1:length(g))){
zp<-as.character(z[which(as.character(z[,15])==g[i]),4])
k[i]<-paste(zp,i,sep="_")
}

a_matrix<-matrix(,nrow=nrow(a),ncol=ncol(a)-1)


for(i in c(1:nrow(a))){
for(j in c(1:ncol(a)-1)){
a_matrix[i,j]<-a[i,j+1]
}
}

colnames(a_matrix)<-k

d=dist(t(a_matrix))

pdf("1_8_single.pdf")
hc1=hclust(d,"single")
plot(hc1)
dev.off()

pdf("1_8_complete.pdf")
hc2=hclust(d,"complete")
plot(hc2)
dev.off()

pdf("1_8_median.pdf")
hc3=hclust(d,"median")
plot(hc3)
dev.off()


pdf("1_8_averge.pdf")
hc4=hclust(d,"average")
plot(hc4)
dev.off()

