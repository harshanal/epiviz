.onLoad <- function(...){
  quietly <- getOption('quietly')
  options(quietly = T)

  options(showWarnCalls=F)
  options(quietly = quietly)

}
