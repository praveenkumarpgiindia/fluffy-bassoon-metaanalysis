metapropbyvarpdf<-function(data,author,year,event_1,total_1,nameevent1,briefnameevent1,event_2,total_2,nameevent2,briefnameevent2,method,studynameinbrief,widthbyvar,heightbyvar,widthbin,heightbin){
  library(meta)
  authornameyear= paste(data[[author]], data[[year]],sep = " ")
  data[["authornameyear"]]=authornameyear
  data=data[order(data[[year]]),]
  # View(data)
  data_1<-data[,c("authornameyear",event_1,total_1)]
  data_1<-data_1[!is.na(data_1[[event_1]]),]
  data_1[,"group"]=nameevent1
  colnames(data_1)<-c("author","n", "N", "group")
  
  data_2<-data[,c("authornameyear",event_2,total_2)]
  data_2<-data_2[!is.na(data_2[[event_2]]),]
  data_2[,"group"]=nameevent2
  colnames(data_2)<-c("author","n", "N", "group")
  
  data_byvar<-rbind(data_1,data_2)
  data_byvar<-data.frame(data_byvar)
  
  metaprop_byvar<-metaprop(event = n, n = N, data_byvar, comb.random=TRUE,comb.fixed = FALSE, studlab=author,pscale = 100,method = method,byvar   = group)
  
  pdf(paste(event_1,"_",event_2,"_propbyvar_",studynameinbrief,".pdf",sep=""),width = widthbyvar,height = heightbyvar)
  forest(metaprop_byvar,layout="Revman")
  dev.off()
  
  data_3=data[!is.na(data[[event_1]])&!is.na(data[[event_2]]),]
  metabin_compare<-metabin(event.e = data_3[[event_1]],n.e = data_3[[total_1]],event.c = data_3[[event_2]],n.c = data_3[[total_2]],studlab = data_3[[author]],method = method,sm = "OR",comb.fixed = FALSE,comb.random = TRUE,label.e = briefnameevent1,label.c = briefnameevent2)
  
  pdf(paste(event_1,"_",event_2,"_bin_",studynameinbrief,".pdf",sep=""),width = widthbin,height = heightbin)
  forest(metabin_compare,layout="Revman")
  dev.off()
  
}
