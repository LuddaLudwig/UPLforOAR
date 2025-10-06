#' Selects best performer from emissions data
#' @param data Data.frame or tibble with columns for emissions (numeric)
#' and sources (character or factor)
#' @returns Data set (tibble) containing the emissions data for the best
#' performing source for New Source Performance Standards.
#' @export
MACT_NSPS=function(data){
  if (("emissions" %in% names(data))==FALSE){
    stop("data must have numeric column named 'emissions' ")
  }
  if (("sources" %in% names(data))==FALSE){
    stop("data must have character or factor column named 'sources' ")
  }
  if (!is.numeric(data$emissions)){
    stop("Emissions must be numeric vector")
  }
  if ((!is.character(data$sources))&(!is.character(data$sources))){
    stop("Sources must be a character or factor vector")
  }
  dat_means=dplyr::summarize(data,means=mean(emissions),.by='sources')
  top_list=dat_means[order(dat_means$means,decreasing=F),]
  top_source=top_list$sources[1]
  dat_top=subset(data,data$sources==top_source)
  dat_top$sources=as.factor(dat_top$sources)
  dat_top$sources=droplevels(dat_top$sources)
  return(dat_top)
}
