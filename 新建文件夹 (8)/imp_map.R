for(i in c(1:9)){
for(j in c(1:30)){



data_first_list_file_name<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/",i,sep="")
data_first_list_file_name<-paste(data_first_list_file_name,"/data_kmeans_sum_",sep="")
data_first_list_file_name<-paste(data_first_list_file_name,j,sep="")
data_first_list_file_name<-paste(data_first_list_file_name,".txt",sep="")

data_zero_list<-read.delim(data_first_list_file_name)

data_zero_list<-data_zero_list[!duplicated(data_zero_list$names), ]
data_zero_list$class<-paste(i,data_zero_list$class,sep="_")
data_zero_list$class<-paste(j,data_zero_list$class,sep="_")
data_zero_list<-na.omit(data_zero_list)
print(data_zero_list)
if(j==1){
data_first_list<-data_zero_list
}else{
data_first_list<-rbind(data_first_list, data_zero_list)
}



}##文件夹内部拼接





if(i==1){
data_second_list<-data_first_list
}else{
data_second_list<-rbind(data_second_list,data_first_list)
}

}##拼接完成数据集合的数据

len_data_second_list<-length(data_second_list[,1])
rownames_data_second_list<-c(1:len_data_second_list)
rownames(data_second_list)<-rownames_data_second_list

write.table(data_second_list,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/imp_beta_map.txt",sep="\t")
