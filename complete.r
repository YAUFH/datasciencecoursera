complete <-function(directory, id=1:332){
  files_full<-list.files(directory, full.names= TRUE)
  id_nobs<-data.frame(id=id, nobs=0)
  for (i in id){
    filex<-read.csv(files_full[i])
    nobs<-sum(complete.cases(filex))
    id_nobs[i,"nobs"]<-nobs
    }
  id_nobs
}
corr<-function(directory, threshold=0){
  complete_cases<-complete(directory)
  complete_cases_threshold<-complete_cases[complete_cases["nobs"]>threshold,]$id
  correlation = numeric()
  
  for(i in complete_cases_threshold){
    newread<-read.csv(paste(directory,"/",formatC(i,width=3,flag='0'),".csv",sep=""))
    dff<-newread[complete.cases(newread),]
    correlation=c(correlation, cor(dff$sulfate, dff$nitrate))
  }
  return(correlation)
  

}