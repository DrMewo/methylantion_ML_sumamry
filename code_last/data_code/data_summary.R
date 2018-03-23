for(i in c(1:9)){
for(j in c(1:30)){

#for(i in c(1:2)){
#for(j in c(1:2)){
all_para_i_j_path<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/",i,sep="")
all_para_i_j_path<-paste(all_para_i_j_path,"/data_kmeans_",sep="")
all_para_i_j_path<-paste(all_para_i_j_path,j,sep="")
all_para_i_j_path<-paste(all_para_i_j_path,".txt",sep="")
#print(all_para_i_j_path)
all_para_i_j<-read.delim(all_para_i_j_path)
all_para_i_j[,2]<-paste(i,j,all_para_i_j[,2],sep="_")
all_para_i_j<-all_para_i_j[,-3]
a_i_j<-length(table(all_para_i_j[,2]))
if(j==1){
all_i_j<-all_para_i_j
a_i<-a_i_j
}else{
all_i_j<-rbind(all_i_j,all_para_i_j)
a_i<-a_i+a_i_j
}
}
if(i==1){
all_i<-all_i_j
a_len<-a_i
}else{
all_i<-rbind(all_i,all_i_j)
a_len<-a_len+a_i
}
}

print(a_len)
write.table(all_i,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/model_last/model_data/all_class.txt",sep="\t")

