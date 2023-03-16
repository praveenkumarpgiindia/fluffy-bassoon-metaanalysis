
metaMeanSDpdf<-function(data,author,year,n.e,mean.e,sd.e,n.c, mean.c,sd.c,method,sm,label.e,label.c,studynameinbrief,width = width,height = height){
  library(meta)
  authornameyear= paste(data[[author]], data[[year]],sep = " ")
  data[["authornameyear"]]=authornameyear
  data=data[order(data[[year]]),]
  # View(data)
  data=data[(!is.na(data[[mean.e]]) & !is.na(data[[mean.c]])),]
  # View(data)
  mentacont=metacont(n.e = data[[n.e]],mean.e = data[[mean.e]],sd.e = data[[sd.e]],n.c = data[[n.c]],mean.c = data[[mean.c]],sd.c = data[[sd.c]],studlab = data[["authornameyear"]],method.smd = method,sm = sm,comb.fixed = FALSE,comb.random = TRUE,label.e = label.e,label.c = label.c)
  pdf(paste(mean.e,"_",mean.c,"_",sm,"_",n.e,"_",n.c,"_",studynameinbrief,".pdf",sep=""),width = width,height = height)
  forest(mentacont,layout="Revman")
  dev.off()
  
}