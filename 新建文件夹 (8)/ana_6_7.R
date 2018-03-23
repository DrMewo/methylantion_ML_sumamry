z<-read.delim("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/list_2.txt")
z[,15] <-sapply(z[,15],function(x) gsub("-",".",x,perl = TRUE))

for(k in c(1:30)){
     para_name<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/7/data",k,sep="_")
     para_name<-paste(para_name,".txt",sep="")
     first_data<-read.delim(para_name)
     first_data [is.na(first_data )] <- 0
     f<-t(first_data [,-1])
     genename<-first_data [,1]
     g<-f[,1]
     ##########ÕûÌåÑ­»·##########
     all_len<-length(genename)

     for(j in c(1:all_len)){
          genename_para<-genename[j]
          h<-data.frame(names(g),f[,j])
          colnames(h)<-c("file_name","num")
          e<-merge(z,h,x.ref="file_name",y.ref="file_name")
            e[which(e[,3]=="c00"),3]<-"not reported"
            e[which(e[,3]=="c05"),3]<-"not reported"
            e[which(e[,3]=="c07"),3]<-"not reported"
            e[which(e[,3]=="c08"),3]<-"not reported"
            e[which(e[,3]=="c24"),3]<-"not reported"
            e[which(e[,3]=="c40"),3]<-"not reported"
            e[which(e[,3]=="c41"),3]<-"not reported"
            e[which(e[,3]=="c43"),3]<-"not reported"
            e[which(e[,3]=="c47"),3]<-"not reported"
            e[which(e[,3]=="c51"),3]<-"not reported"
            e[which(e[,3]=="c63"),3]<-"not reported"
            e[which(e[,3]=="c70"),3]<-"not reported"
            e[which(e[,3]=="c72"),3]<-"not reported"
            e[which(e[,3]=="c75"),3]<-"not reported"
          a<-e[,3]
          d<-c(1:length(a))

        for(i in c(1:length(a))){
             d[i]<-as.character(e[i,3])
        }
            b<-e[,17]
            c<-data.frame(d,b)

            len_part<-length(table(c[,1]))
            len_part_2<-len_part+1
            zp1<-c(1:len_part_2)
            zp2<-c(1:len_part_2)
            zp_table<-data.frame(zp1,zp2)
            colnames(zp_table)<-c("part",as.character(genename_para))
            zp_table[1,1]<-"part"
            zp_table[1,2]<-as.character(genename_para)

            for(i in c(1:len_part)){
            zp_table[i+1,1]<-names(table(c[,1]))[i]
            k1<-c[which(c[,1]==names(table(c[,1]))[i]),2]
            k2<-c[-which(c[,1]==names(table(c[,1]))[i]),2]
            zp_table[i+1,2]<-wilcox.test(k1,k2,exact=FALSE,correct=FALSE)$p.value
            zp_table[is.na(zp_table)] <- 0
            }
            if(j==1){
            all_data<-zp_table
            print(head(all_data))
            }else{
            print(head(all_data))
            print(head(zp_table))
            all_data<-merge(all_data,zp_table,x.by="part",y.by="part")
            }
}
          
para_name_summary<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/7/data_summary",k,sep="_")
para_name_summary<-paste(para_name_summary,".txt",sep="")
write.table(all_data,para_name_summary,sep="\t")

if(k==1){
data_10_all<-all_data
}else{
data_10_all<-merge(data_10_all,all_data,x.by="part",y.by="part")
}

}
para_name_summary_10<-paste("/mnt/ilustre/users/sanger-dev/sg-users/zhangpeng/fighting/data_para/7/data_summary_10",k,sep="_")
para_name_summary_10<-paste(para_name_summary_10,".txt",sep="")
write.table(data_10_all,para_name_summary_10,sep="\t")

