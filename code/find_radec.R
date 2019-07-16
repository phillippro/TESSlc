library(foreach)
library(doMC)
library(parallel)
library(FITSio)
fs <- commandArgs(trailingOnly=TRUE)
fs <- list.files('S9',pattern='fits',full.name=TRUE)
#fs <- sample(fs,1e3)
out <- c()
savef <- FALSE
photo <- c()
Ncores <- 4
#for(f in fs){
if(Ncores>0) {registerDoMC(Ncores)} else {registerDoMC()}
p <- foreach(f = fs) %dopar% {
#      fdat <- gsub('fits','dat',f)
    fdat <- 'ljk'
      if(!file.exists(fdat)){
  cat(f,'\n')
  data <- try(readFITS(file = f, hdu = 1, maxLines = 500000,fixHdr = c('none', 'remove', 'substitute'), phdu = 1),TRUE)
  fout <- gsub('fits','dat',f)
  if(class(data)!='try-error'){
      info <- data$header
      id <- gsub("OBJECT.+=|/.+| |'",'',info[grep('OBJECT',info)])
      ra <- as.numeric(gsub('RA_OBJ  =|/.+','',info[grep('RA_OBJ',info)]))
      dec <- as.numeric(gsub('DEC.+=|/.+','',info[grep('DEC_OBJ',info)]))
      out <- rbind(out,c(f,id,ra,dec))
      if(savef){
          photo <- cbind(data$col[[which(data$colNames=='TIME')]],data$col[[which(data$colNames=='PDCSAP_FLUX')]],data$col[[which(data$colNames=='PDCSAP_FLUX_ERR')]])
          ind <- which(!is.na(photo[,2]) & !is.na(photo[,3]))
          photo <- photo[ind,]
          cat(fout,'\n\n')
          write.table(photo,fout,quote=FALSE,row.names=FALSE,col.names=c('Time','PDCSAP','ePDCSAP'))
      }
#      detach(data)
  }
  }
}
write.table(out,file='s9.txt',quote=FALSE,row.names=FALSE,col.names=FALSE)
