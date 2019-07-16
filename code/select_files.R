tab <- read.csv('s7sim.csv')
info <- read.table('s7info.txt')
ind <- which(tab[,'plx']>10)
out <- data.frame(gsub('.fits','.dat',info[ind,1]),info[ind,2:4],tab[ind,'main_id'])
colnames(out) <- c('file','ID','ra','dec','Name')
write.csv(out,file='s7near.csv',quote=FALSE,row.names=FALSE)

