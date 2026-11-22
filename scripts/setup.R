# Run from the extracted package root; never installs into a global library.
root <- normalizePath(getwd(),winslash='/',mustWork=TRUE)
stopifnot(file.exists(file.path(root,'renv.lock')))
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED='FALSE', RENV_PATHS_ROOT=file.path(root,'renv/cache'), RENV_PATHS_SANDBOX=file.path(root,'renv/sandbox'),RENV_CONFIG_AUTOLOADER_ENABLED='FALSE')
options(timeout=1200,repos=c(CRAN='https://cloud.r-project.org'))
boot <- file.path(root,'renv/bootstrap');dir.create(boot,recursive=TRUE,showWarnings=FALSE)
.libPaths(c(boot,.libPaths()))
if(!file.exists(file.path(boot,'renv/DESCRIPTION'))) {
 url <- 'https://cran.r-project.org/src/contrib/Archive/renv/renv_1.3.1.tar.gz'
 # The pinned release may still be current on CRAN.
 current <- 'https://cran.r-project.org/src/contrib/renv_1.3.1.tar.gz'
 archive <- tempfile(fileext='.tar.gz')
 tryCatch(download.file(current,archive,mode='wb'),error=function(e)download.file(url,archive,mode='wb'))
 install.packages(archive,repos=NULL,type='source',lib=boot)
 unlink(archive)
}
stopifnot(as.character(packageVersion('renv',lib.loc=boot))=='1.3.1')
renv::consent(provided=TRUE)
renv::restore(project=root,library=renv::paths$library(project=root),lockfile=file.path(root,'renv.lock'),prompt=FALSE)
source(file.path(root,'scripts/bootstrap.R'))
source(file.path(root,'scripts/doctor.R'))
