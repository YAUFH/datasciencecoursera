pollutantmean<-function(directory,pollutant,id=1:332){
  files_full<-list.files(directory,full.names=TRUE)
  data<-data.frame()
    for(i in id){
      data<-rbind(data,read.csv(files_full[i]))
    }
  filesMean<-mean(data[[pollutant]], na.rm=TRUE)
  filesMean
  }

