sector <- 9
fs <- list.files(paste0('S',sector),pattern='Robj',full.name=TRUE)
tmp <- c()
k <- 0
for(f in fs){
    k <- k+1
    cat('file',k,':',f,'\n')
    load(f)
    Nsig <- length(Popt$white)
    K <- sqrt(pars$white['A',]^2+pars$white['B',]^2)
    lnBFmax <- sapply(1:Nsig,function(i) max(lnBF$white[,i]))
    out <- cbind(f,signal=paste0('sig',1:Nsig),t(pars$white),K,lnBFmax,Popt=Popt$white)
    tmp <- rbind(tmp,out)
}
write.csv(tmp,file=paste0('s',sector,'period.csv'),quote=FALSE,row.names=FALSE)
