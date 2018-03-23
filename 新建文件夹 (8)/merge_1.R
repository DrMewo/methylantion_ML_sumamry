cancer_table<-function(file_path,out_path){
ab<-list.files(file_path)
first_data_1<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/gdc/9a9bad41-e441-4774-b45d-a863a68d80be/jhu-usc.edu_BRCA.HumanMethylation450.22.lvl-3.TCGA-EW-A1PC-01B-11D-A21R-05.gdc_hg38.txt",header=T)
list_data<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/list.txt",sep= '')
first_data_2<-first_data_1[,c(1:2)]
colnames(first_data_2)<-c("ref","test")
new_data<-first_data_2
#for (i in c(1:2)){
for (i in c(1:length(ab))){
    firstfile<-paste(file_path,ab[i],sep='/')
    if(list.files(firstfile)[1]=="logs")
        {
            secondfile<-list.files(firstfile)[2]
        }
        else{
            secondfile<-list.files(firstfile)[1]
        }
    fileall<-paste(firstfile,secondfile,sep='/')
    data_para<-read.delim(fileall,header=T)
    #print(length(data_para[,1]))
    #print(which(list_data[,15]%in%secondfile))
    print(i)
    if(length(data_para[,1])==485577)
        {
            data_para_1<-data_para[,c(1,2)]
            colnames(data_para_1)<-c("ref",secondfile)
            oldname<-colnames(new_data)
            new_data<-merge(new_data,data_para_1,by="ref")
            colnames(new_data)<-c(oldname,secondfile)
        }
        else{
            print(fileall)
        }
    }

abc<-list.files("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/gdc")
#for (i in c(1:2)){
for (i in c(1:length(abc))){
    print(i)
    firstfile<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/gdc",abc[i],sep='/')
    if(list.files(firstfile)[1]=="logs")
        {
            secondfile<-list.files(firstfile)[2]
        }
        else{
            secondfile<-list.files(firstfile)[1]
        }
    fileall<-paste(firstfile,secondfile,sep='/')
    data_para<-read.delim(fileall,header=T)
    if(length(data_para[,1])==485577)
        {
            data_para_1<-data_para[,c(1,2)]
            colnames(data_para_1)<-c("ref",secondfile)
            oldname<-colnames(new_data)
            new_data<-merge(new_data,data_para_1,by="ref")
            colnames(new_data)<-c(oldname,secondfile)
        }
        else{
            print(fileall)
        }
    }

new_data<-new_data[,-2]
print(head(new_data))
#print(length(new_data[,1]))


new_data<-na.omit(new_data)
name_1<-new_data[,1]
name_2<-t(name_1)
new_data_one<-new_data
##write.table(new_data_one,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_10.txt",row.names = FALSE,sep= '\t')
write.table(new_data_one,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_8.txt",row.names = FALSE,sep= '\t')
new_data_one_t<-t(new_data_one)
write.table(new_data_one_t,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_9.txt",row.names = TRUE,col.names= FALSE,sep= '\t')
new_data<-new_data[,-1]
name_3<-colnames(new_data)
new_data_t<-t(new_data)
colnames(new_data_t)<-name_2
rownames(new_data_t)<-name_3

write.table(name_2,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_10.txt",sep= '\t')
write.table(name_3,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_11.txt",sep= '\t')

print(new_data_t[,1:10])

write.table(new_data,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_6.txt",row.names = TRUE,sep= '\t')
write.table(new_data_t,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_7.txt",row.names = TRUE,sep= '\t')
#write.table(new_data,out_path,row.names = FALSE)
}




cancer_table("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/gdc","/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_4.txt")

#grep211728.out


#para_data_one<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_1000.txt",sep= '')
#print("a")
#print(head(para_data_one))

#para_data_one_round<-para_data_one

#print(length(para_data_one[1,]))


#for (i in c(2:length(para_data_one[1,]))){
#print(para_data_one[,i])
#para_data_one_round[,i]<-round(para_data_one[,i],5)
#}

#write.table(para_data_one_round,"/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_1000_round.txt",row.names = FALSE)


#para_data_1<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_1000.txt",sep= '\t')
#para_data_1[1:5,1:5]
#head(para_data_1)
#str(para_data_1)
para_data_2<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_2000.txt",sep= '\t')
#para_data_3<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_3000.txt",sep= '\t')
#para_data_4<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data/zp_4000.txt",sep= '\t')
#para_data_5<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/zp_2_1000.txt",sep= '\t')
#para_data_6<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/zp_2_2000.txt",sep= '\t')
#para_data_7<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/zp_2_3000.txt",sep= '\t')
#para_data_8<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/zp_2_4000.txt",sep= '\t')
#para_data_9<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/zp_2_5000.txt",sep= '\t')
#para_data_10<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/cancer_data_part2/zp_2_6000.txt",sep= '\t')

#para_data_list<-c(para_data_1,para_data_2,para_data_3,para_data_4,para_data_5,para_data_6,para_data_7,para_data_8,para_data_9,para_data_10)
#para_data_list<-c(para_data_1,para_data_2)


#para_data_one<-para_data_list[1]
#for(i in c(2:length(para_data_list))){
#print(i)
#one_names<-colnames(para_data_one)
#para_data_two<-para_data_list[i]
#two_names<-colnames(para_data_two)
#para_data_one<-merge(para_data_one,para_data_two,by="ref")
#merge_name<-c(one_names,two_names[-1])
#colnames(para_data_one)<-merge_name
#}


#para_data_one<-na.omit(para_data_one)
#write.table(para_data_one,/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_1_t_n_test.txt,row.names = FALSE,sep= '\t')

#write.table(t(para_data_one),/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_1.txt,row.names = FALSE)

#data_all <-t(para_data_one)

#write.table(data_all,/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_all/data_2_t_n_test.txt,row.names = FALSE,sep= '\t')





