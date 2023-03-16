metapropreadybyvarpdf<-function(data,author,year,event,total,method,byvar,studynameinbrief,width = width,height = height){
  library(meta)
  authornameyear= paste(data[[author]], data[[year]],sep = " ")
  data[["authornameyear"]]=authornameyear
  data=data[order(data[[year]]),]
  # View(data)
  data=data[!is.na(data[[event]]),]
  # View(data)
  metaprop_readybyvar=metaprop(event = data[[event]],n = data[[total]],byvar = data[[byvar]],studlab = data[["authornameyear"]],method = method,comb.fixed = FALSE,comb.random = TRUE,pscale = 100)
  # print(summary(metaprop_Calp))
  pdf(paste(event,"_",byvar,"_propreadybyvar_",total,"_",studynameinbrief,".pdf",sep=""),width = width,height = height)
  forest(metaprop_readybyvar,layout="Revman")
  dev.off()
  
}

