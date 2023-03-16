metaproppdf<-function(data,author,year,event,total,method,studynameinbrief,width = width,height = height){
  library(meta)
  authornameyear= paste(data[[author]], data[[year]],sep = " ")
  data[["authornameyear"]]=authornameyear
  data=data[order(data[[year]]),]
  # View(data)
  data=data[!is.na(data[[event]]),]
  # View(data)
  metaprop=metaprop(event = data[[event]],n = data[[total]],studlab = data[["authornameyear"]],method = method,comb.fixed = FALSE,comb.random = TRUE,pscale = 100)
  # print(summary(metaprop_Calp))
  pdf(paste(event,"_prop_",total,"_",studynameinbrief,".pdf",sep=""),width = width,height = height)
  forest(metaprop,layout="Revman")
  dev.off()
  
}
