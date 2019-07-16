library(magicaxis)
library(foreach)
library(doMC)
library(parallel)
source('periodograms.R')
source('periodoframe.R')
source('mcmc_func.R')
options(warn=2)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)>0){
    ind <- as.integer(args[1])
}else{
    ind <- 1
}
Nmax <- 3
ofac <- 2
#    noise.types <- c('white','MA','AR')#,'GP011')
noise.types <- c('white')
per.type <- 'GLST'
fs <- as.character(read.csv('s9file.txt')[,1])
Ncores <- 8
binning <- TRUE
if(Ncores>0) {registerDoMC(Ncores)} else {registerDoMC()}
p <- foreach(f = fs[(ind-1)*100+(1:100)]) %dopar% {
#for(f in fs[1]){
    pq <- 1
    Nsamp <- 1
    tab <- try(read.table(f,header=TRUE),TRUE)
                                        #tab <- tab[1:1000,]
    if(class(tab)!='try-error'){
        ts <- tab[,1]-min(tab[,1])
        flux <-  tab[,2]-mean(tab[,2])
        eflux <-  tab[,3]
        ts0 <- 2450000+min(tab[,1])
        if(binning){
            tmp <- wtb(ts,flux,eflux,dt=0.01)
            ts <- tmp[,1]-min(tmp[,1])
            flux <- tmp[,2]
            eflux <- tmp[,3]
            ts0 <- ts0+min(tmp[,1])
        }

        sigmaGP <- sd(flux)

        Nmas <- Nars <- durs <- c()
        pars <- lnBF <- Popt <- list()
        fmin <- 1/max(100,max(ts)-min(ts))
        fmax <- 1/(10*min(diff(ts)))
        Popt <- c()
        for(j in 1:length(noise.types)){
            noise.type <- noise.types[j]
            cat('noise.type=',noise.type,'\n')
            if(grepl('MA',noise.type)){
                Nma <- pq
            }else{
                Nma <- 0
                                        #        Nma <- 1
            }
            if(grepl('AR',noise.type)){
                Nar <- pq
            }else{
                Nar <- 0
            }
            Nars <- c(Nars,Nar)
            Nmas <- c(Nmas,Nma)
            gp.par <- rep(NA,3)
            if(grepl('GP',noise.type)){
                GP <- TRUE
                tmp <- as.integer(gsub('GP|\\+.+','',noise.type))
                if(floor(tmp/100)){
                    gp.par[1] <- sigmaGP
                }
                if(floor((tmp%%100)/10)){
                    gp.par[2] <- logProt
                }
                if(floor(tmp%%10)){
                    gp.par[3] <- logtauGP
                }
            }else{
                GP <- FALSE
            }
            lnBF1 <- Popt1 <- par1 <- out1 <- c()
            for(k in 1:Nmax){
                t1 <- proc.time()
                if(k==1){
                    res <- flux
                }else{
                    res <- per$res
                }
                Indices <- NULL
###first find the baseline optimal parameters
                par.opt <- NULL
                if(per.type=='BFP'){
                    per <- BFP(ts,res,eflux,Nma=Nma,Nar=Nar,Indices=Indices,ofac=ofac,model.type='man',fmin=fmin,fmax=fmax,quantify=TRUE,progress=FALSE,GP=GP,gp.par=gp.par,noise.only=FALSE,Nsamp=Nsamp,sampling='combined',par.opt=par.opt,renew=TRUE)
                }else if(per.type=='GLST'){
                    per <- glst(t=ts,y=res,err=eflux,fmax=fmax,ofac=ofac,fmin=fmin)
                }
                dur <- (proc.time()-t1)[3]
                cat('computation time for ',noise.type,' for ',k,'signal is ',dur,'s\n')
                lnBF1 <- cbind(lnBF1,per$lnBFs)
                par1 <- cbind(par1,unlist(per$par.opt))
                Popt1 <- c(Popt1,per$P[which.max(per$lnBFs)])
            }
            lnBF[[noise.type]] <- lnBF1
            pars[[noise.type]] <- par1
            Popt[[noise.type]] <- Popt1
        }
####plot
        fout <- gsub('.dat',paste0('_',per.type,'.pdf'),f)
        cat('output pdf:\n',fout,'\n')
        pdf(fout,16,16)
        par(mfrow=c(4,4))
        for(k in 1:length(noise.types)){
            noise.type  <- noise.types[k]
            res <- flux
            for(j in 1:Nmax){
                par.opt <- pars[[noise.type]][,j]
                Pmax <- Popt[[noise.type]][j]
                tsim <- seq(min(ts),max(ts),length.out=1e3)
                ysim <- par.opt[1]*cos(2*pi/Pmax*tsim)+par.opt[2]*sin(2*pi/Pmax*tsim)+par.opt[3]+par.opt[4]*tsim
###raw data plot
                plot(ts,res,xlab=paste0('BJD-',ts0),ylab='dFlux',pch=20,cex=0.2,ylim=c(mean(res)-5*sd(res),mean(res)+5*sd(res)))
                legend('topright',bty='n',legend=paste0('RMS=',round(sd(res),2)))
###binned data
                if(!binning){
                    tmp <- wtb(ts,res,tab[,3],dt=0.01)
                    plot(tmp[,1],tmp[,2],xlab=paste0('BJD-',ts0),ylab='dFlux',pch=20)
                }else{
                    tf <- ts
                    plot(tf,res,xlab=paste0('BJD-',ts0),ylab='dFlux',pch=20,cex=0.2,col=tcol('grey',50),ylim=c(mean(res)-5*sd(res),mean(res)+5*sd(res)))
                }
                lines(tsim,ysim,col='red')
                Kmax <- sqrt(sum(par.opt[1:2]^2))
                legend('topright',legend=paste0('P=',round(Pmax,2),'d; K=',round(Kmax,2)),bty='n',col='red')
                ##phase curve
                tf <- ts%%Pmax
                ysim <- par.opt[1]*cos(2*pi/Pmax*tsim)+par.opt[2]*sin(2*pi/Pmax*tsim)
                plot(tf,res-(par.opt[3]+par.opt[4]*ts),xlab='Phase [day]',ylab='dFlux',pch=20,cex=0.2,col=tcol('grey',50),ylim=c(mean(res)-5*sd(res),mean(res)+5*sd(res)))
                points(tsim%%Pmax,ysim,col='red',cex=0.1)
                Kmax <- sqrt(sum(par.opt[1:2]^2))
                legend('topright',legend=paste0('Detrended;P=',round(Pmax,2),'d; K=',round(Kmax,2)),bty='n',col='red')
                ##corresponding periodogram
                ylim <- range(lnBF[[noise.type]][,j],0,5)
                plot(per$P,lnBF[[noise.type]][,j],xlab='P [d]',ylab='ln(BF)',main=paste0('raw-',j-1,'signal; ',per.type,'; ',noise.type,'; p=',Nars[k],';q=',Nmas[k]),log='x',type='l',ylim=ylim,xaxt='n')
                magaxis(side=1,tcl=-0.5)
                abline(h=c(0,5),lty=3:2,col='grey')
                Pmax <- Popt[[noise.type]][j]
                abline(v=Pmax,col='red',lty=3,lwd=3)
                legend('topright',legend=paste0('P=',round(Pmax,2),'d'),bty='n',col='red')
###update res
                yp <- par.opt[1]*cos(2*pi/Pmax*ts)+par.opt[2]*sin(2*pi/Pmax*ts)+par.opt[3]+par.opt[4]*ts
                res <- res-yp
            }
###combined fit
            tsim <- seq(min(ts),max(ts),length.out=1e3)
            ysim <- 0
            for(j in 1:Nmax){
                par.opt <- pars[[noise.type]][,j]
                Pmax <- Popt[[noise.type]][j]
                ysim <- ysim+(par.opt[1]*cos(2*pi/Pmax*tsim)+par.opt[2]*sin(2*pi/Pmax*tsim)+par.opt[3]+par.opt[4]*tsim)
            }
            plot(ts,flux,xlab='Phase [day]',ylab='dFlux',pch=20,cex=0.2,col=tcol('grey',50),ylim=c(mean(res)-5*sd(res),mean(res)+5*sd(res)),main='combined fit')
            lines(tsim,ysim,col='red')
            Kmax <- sqrt(sum(par.opt[1:2]^2))
            dev.off()

###save data
            Ps <- per$P
            fobj <- gsub('.dat',paste0('_',per.type,'.Robj'),f)
            cat(fobj,'\n\n')
            save(Ps,lnBF,pars,Popt,noise.types,file=fobj)
        }
    }
}
