# Load the isolated project library when setup has created it.
root <- Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd())
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED='FALSE', RENV_PATHS_ROOT=file.path(root,'renv/cache'), RENV_PATHS_SANDBOX=file.path(root,'renv/sandbox'), RENV_CONFIG_AUTOLOADER_ENABLED='FALSE')
if(dir.exists(file.path(root,'renv/library'))) {
 .libPaths(c(file.path(root,'renv/bootstrap'),.libPaths()))
 if(!requireNamespace('renv',quietly=TRUE))stop('Run Rscript --vanilla scripts/setup.R first.')
 lib <- renv::paths$library(project=root)
 if(!dir.exists(lib))stop('No library for this R/platform. Run scripts/setup.R on this computer.')
 .libPaths(c(lib,.Library))
}
