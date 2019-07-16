source('mcmc_func.R')
sector <- 7
tab <- read.csv(paste0('s',sector,'final.csv'))
pdf(paste0('S',sector,'periods.pdf'),16,16)
ind <- which(tab[,'signal']=='sig1')
tab <- tab[ind,]
if(sector==9){
index <- grep('86226',tab[,'main_id'])
}
types <- as.character(unique(tab[,'sp_type']))
maintype <-  c('A','F','G','K','M','L','D')
minortype <- c()
for(type in types){
    tmp <- unlist(strsplit(type,''))
    if(length(tmp)>2){
        minortype <- c(minortype,paste(tmp[1:2],collapse=''))
    }else{
        minortype <- c(minortype,type)
    }
}

inds <- sapply(maintype,function(type) grep(paste0('^',type),as.character(tab[,'sp_type'])))
x <- lapply(1:length(inds),function(i) rep(maintype[i],length(inds[[i]])))
y <- lapply(1:length(inds),function(i) mean(tab[inds[[i]],'Popt']))
x1 <- lapply(1:length(inds),function(i) rep(i,length(inds[[i]])))
inds <- unlist(inds)
x <- unlist(x)
x1 <-unlist(x1)
fout <- paste0('type_period_S',sector,'.pdf')
cat(fout,'\n')
pdf(fout,8,8)
par(mfrow=c(2,2))
plot(x1,tab[inds,'Popt'],xlab='Stellar type',ylab='period',log='y',xaxt='n',main='Stellar type v.s. Period',col=tcol('black',50))
if(sector==9){
points(3,tab[index,'Popt'],col='green',pch=20)
}
axis(side=1,at=1:length(maintype),label=maintype)
points(1:length(maintype),y,col='red',pch=20,cex=1)
if(sector==9){
legend('top',xpd=NA,inset=c(0,-0.1),legend=c('Average period','HD86226'),col=c('red','green'),pch=20,horiz=TRUE,bty='n')
}
##
plot(1000/tab[,'plx'],tab[,'Popt'],xlab='D [pc]',ylab='P [day]',log='y',main='Distance v.s. Period')
if(sector==9){
points(1000/tab[index,'plx'],tab[index,'Popt'],col='green',pch=20)
}
##
if(sector==9){
    xlim <- c(-1e2,1e2)
}else{
    xlim <- range(tab[,'B']-tab[,'V'],na.rm=TRUE)
}
plot(tab[,'B']-tab[,'V'],tab[,'Popt'],xlab='B-V [mag]',ylab='P [day]',log='y',main='B-V v.s. Period',xlim=xlim)
if(sector==9){
points(tab[index,'B']-tab[index,'V'],tab[index,'Popt'],col='green',pch=20)
}
plot(tab[,'V']-tab[,'R'],tab[,'Popt'],xlab='V-R [mag]',ylab='P [day]',log='y',main='V-R v.s. Period',xlim=c(-2,2))
if(sector==9){
points(tab[index,'V']-tab[index,'R'],tab[index,'Popt'],col='green',pch=20)
}
title(main=paste(nrow(tab),'stars in sector',sector,'with d<100 pc'),line=-1,outer=TRUE)
dev.off()
