a<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all.txt")
e<-a[,1]
f<-a[,-1]
rownames(f)<-e
g<-t(f)
z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")
z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))
n<-z[,c(2,15)]
h<-merge(g,n,x.by=colnames(g),y.by="file_name")
write.table(h,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/cal_data_all_merge.txt",sep="\t")
print(head(h))