#fs <- list.files('S7','fits',full.name=TRUE)
fs <- list.files('S9','fits',full.name=TRUE)
for(f in fs){
  system(paste('rm',f))
}