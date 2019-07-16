fs <- list.files('S9',pattern='header',full.name=TRUE)
out <- c()
for(f in fs){
    f0 <- gsub('header','.dat',f)
    cat(f,'\n')
    info <- readLines(f)
    id <- gsub("OBJECT.+=|/.+| |'",'',info[grep('OBJECT',info)])
    ra <- as.numeric(gsub('RA_OBJ  =|/.+','',info[grep('RA_OBJ',info)]))
    dec <- as.numeric(gsub('DEC.+=|/.+','',info[grep('DEC_OBJ',info)]))
    out <- rbind(out,c(f0,id,ra,dec))
}
write.table(out,file='s9.txt',quote=FALSE,row.names=FALSE,col.names=FALSE)
