file_path<-"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/gdc"

ab<-list.files(file_path)

#map_list<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/imp_beta_map.txt")
##first_data_1<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/gdc/9a9bad41-e441-4774-b45d-a863a68d80be/jhu-usc.edu_BRCA.HumanMethylation450.22.lvl-3.TCGA-EW-A1PC-01B-11D-A21R-05.gdc_hg38.txt",header=T)
list_data<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/list.txt",sep= '')
##first_data_2<-first_data_1[,c(1:2)]
##colnames(first_data_2)<-c("ref","test")
##new_data<-first_data_2
##for (i in c(1:2)){
for (i in c(1:length(ab))){
    firstfile<-paste(file_path,ab[i],sep='/')
    if(list.files(firstfile)[1]=="logs")
        {
            secondfile<-list.files(firstfile)[2]
        } else{
            secondfile<-list.files(firstfile)[1]
        }
    fileall<-paste(firstfile,secondfile,sep='/')
    data_para<-read.delim(fileall,header=T)
    #print(length(data_para[,1]))
    #print(which(list_data[,15]%in%secondfile))
    if(length(data_para[,1])==485577)
        {
            data_para_1<-data_para[,c(1,2)]
            colnames(data_para_1)<-c("names","Beta_value")
            if(i==1){
            #data_para_1<-merge(data_para_1,map_list,x.by="names",y.by="names")
                data_para_1[is.na(data_para_1)] <- 0
               data_para_2<-data_para_1
               colnames(data_para_2)<-c("names","means")
            }else{
               data_para_1<-data_para[,c(1,2)]
               colnames(data_para_1)<-c("names","Beta_value")
               data_para_1[is.na(data_para_1)] <- 0
               #oldnames<-colnames(data_para_2)
               data_para_2<-merge(data_para_2,data_para_1,x.by="names",y.by="names")
               #colnames(data_para_2)<-c(oldnames,secondfile)
               data_para_2$sum<-(data_para_2[,2]+data_para_2[,3])/2
               data_para_2<-data_para_2[,-2]
               data_para_2<-data_para_2[,-2]
               colnames(data_para_2)<-c("names","means")
               print(i)
               }
            
            #colnames(data_para_1)<-c("names",secondfile)
            #oldname<-colnames(new_data)
            #new_data<-merge(new_data,data_para_1,by="ref")
            #colnames(new_data)<-c(oldname,secondfile)
        } else{
            print(fileall)
        }
    }
new_data<-data_para_2
print(head(new_data))
#print(length(new_data[,1]))

write.table(new_data,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/gogo/sum_1.txt",row.names = FALSE)
