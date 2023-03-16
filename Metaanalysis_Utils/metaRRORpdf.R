metaRRORpdf<-function(data,author,year,event.e,n.e,event.c,n.c,method,sm,label.e,label.c,studynameinbrief,width = width,height = height){
  library(meta)
  authornameyear= paste(data[[author]], data[[year]],sep = " ")
  data[["authornameyear"]]=authornameyear
  data=data[order(data[[year]]),]
  # View(data)
  data=data[(!is.na(data[[event.e]]) & !is.na(data[[event.c]])),]
  # View(data)
  metaRR=metabin(event.e = data[[event.e]],event.c = data[[event.c]],n.e = data[[n.e]],n.c=data[[n.c]],
                 studlab = data[["authornameyear"]],method = method,sm = sm,comb.fixed = FALSE,comb.random = TRUE,label.e = label.e,label.c = label.c)
  pdf(paste(event.e,"_",event.c,"_",sm,"_",studynameinbrief,".pdf",sep=""),width = width,height = height)
  forest(metaRR,layout="Revman")
  dev.off()
  
}

