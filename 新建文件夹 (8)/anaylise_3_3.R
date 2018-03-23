z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")

z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))

a<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_10_3_ana.txt")

for(i in c(1:30)){
len<-length(a[,1])
lenone<-floor(len*((i-1)/(30))+1)
lentwo<-floor(len*(i/(30)))
a_para<-a[lenone:lentwo,]
para_name<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/3/data",i,sep="_")
para_name<-paste(para_name,".txt",sep="")
write.table(a_para,para_name,sep="\t")
}


