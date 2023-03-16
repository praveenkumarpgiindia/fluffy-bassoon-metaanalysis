metapropWithoutyearpdf<-function(data,author,event,total,method,studynameinbrief,width = width,height = height){
  library(meta)
  authorname= data[[author]]
  data[["authorname"]]=authorname
  data=data[order(data[[author]]),]
  # View(data)
  data=data[!is.na(data[[event]]),]
  # View(data)
  metaprop=metaprop(event = data[[event]],n = data[[total]],studlab = data[["authorname"]],method = method,comb.fixed = FALSE,comb.random = TRUE,pscale = 100)
  # print(summary(metaprop_Calp))
  pdf(paste(event,"_prop_",total,"_",studynameinbrief,".pdf",sep=""),width = width,height = height)
  forest(metaprop,layout="Revman")
  dev.off()
  
}
