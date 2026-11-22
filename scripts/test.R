source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'scripts/bootstrap.R'))
root <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd())
setwd(root)
Sys.setenv(R_LIBS=paste(.libPaths(),collapse=.Platform$path.sep))
tests <- list.files('tests',pattern='^test_.*\\.R$',full.names=TRUE)
for(f in tests) {
 cat('\nTEST',f,'\n')
 # Each test has its own environment/process so fixture path changes cannot leak.
 status <- system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(f)))
 if(status!=0L)stop('Failed: ',f)
}
cat('All synthetic R tests passed.\n')
