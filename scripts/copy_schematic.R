source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR',unset=getwd()),'project_helpers_and_constants.R'))
stopifnot(file.copy(file.path(CODE_DIR,'assets/spatial_sub_for_paper.png'),file.path(FIGURES_DIR,'spatial_sub_for_paper.png'),overwrite=TRUE))
