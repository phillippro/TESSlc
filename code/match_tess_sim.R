tab <- read.table('s9.txt')
sim <- read.csv('s9sim.csv')
colnames(tab) <- c('file','ID','RA','DEC')
inds <- c()
for(j in 1:nrow(tab)){
#for(j in 1:1000){
    if(j%%1000==0) cat(j,'\n')
    ra <- tab[j,3]
    dec <- tab[j,4]
    ind <- which(sim[,'RA']==ra & sim[,'DEC']==dec)
#    cat('length(ind)=',length(ind),'\n')
    if(length(ind)>0){
        ii <- ind[which.min(sim[ind,'angDist'])]
        inds <- rbind(inds,c(j,ii))
    }
}
out <- data.frame(tab[inds[,1],1:2],sim[inds[,2],])
write.csv(out,file='s9info.csv',row.names=FALSE,quote=FALSE)

ind <- which(out[,'plx']>10)
write.table(gsub('\\.\\.','\\.',out[ind,'file']),file='s9file.txt',quote=FALSE,row.names=FALSE,col.names=FALSE)
