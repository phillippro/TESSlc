#sector <- '9'
sector <- 7
tab <- read.csv(paste0('s',sector,'period.csv'))#periods of TESS light curves
if(sector==9){
data <- read.csv(paste0('s',sector,'info.csv'))#files with target information
}else{
data <- read.csv(paste0('s',sector,'sim.csv'))#files with target information
}
#sim <- read.csv(paste0('s',sector,'sim.csv'))
fs <- unique(gsub('_GLST.Robj','',tab[,1]))
if(sector==9){
f <- gsub('..dat','',data[,1])
}else{
f <- gsub('.dat','',data[,1])
}
ind <- match(fs,f)
ii <- which(!is.na(ind))
out <- c()
for(j in ii){
    if(j%%100==0) cat('j=',j,'\n')
    ind1 <- grep(fs[j],tab[,1])
    ind2 <- grep(fs[j],data[,1])
#    ind2 <- which(data[ind2,'Name']==sim[,'main_id'])
    if(FALSE){
        cat('ind1=',ind1,'\n')
        cat('ind2=',ind2,'\n')
    }
    ind <- which(tab[ind1,'lnBFmax']>5)
    if(length(ind)>0){
        out <- rbind(out,data.frame(tab[ind1,],data[ind2,-1]))
    }
}
write.csv(out,file=paste0('s',sector,'final.csv'),quote=FALSE,row.names=FALSE)
