a<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all.txt")
e<-a[,1]
f<-a[,-1]
rownames(f)<-e

g<-as.data.frame(t(f))
z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")
z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))
n<-z[,c(2,15)]
g$file_name<-rownames(g)
h<-merge(g,n,x.by="file_name",y.by="file_name")
#write.table(h,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge.txt",sep="\t")
#print(head(h))
#m<-data.frame(rownames(g),c(1:length(rownames(g))))
#head(m)
#colnames(m)<-c("file_name","number")
#h<-merge(m,n,x.by="file_name",y.by="file_name")
#write.table(h,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge.txt",sep="\t")
#print(head(h))
#h<-h[,c(1,3)]
#print(head(h))
#print(head(g))
#y<-cbind(g,h)
#print(head(y))
write.table(h,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge_1.txt",sep="\t",row.name=FALSE)

