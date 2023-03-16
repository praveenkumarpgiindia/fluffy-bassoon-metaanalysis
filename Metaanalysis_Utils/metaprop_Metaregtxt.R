metaprop_Metaregtxt<-function(data,author,year,event,total,covariate,studynameinbrief,width = width,height = height){
  library(meta)
  library(metafor)
  authornameyear= paste(data[[author]], data[[year]],sep = " ")
  data[["authornameyear"]]=authornameyear
  data=data[order(data[[year]]),]
  # View(data)
  data=data[!is.na(data[[event]]) & !is.na(data[[covariate]]),]
  # View(data)
  ess =escalc(measure = "PLO",xi = data[[event]],ni = data[[total]])
  meta <-rma(ess$yi,ess$vi,mods = ~data[[covariate]],method = "ML")
  capture.output(print(meta),file = paste("Metaregression/","Metareg_",event,"_",covariate,"_",studynameinbrief,".txt",sep = ""))
}


