#' Selects best performer from emissions data
#' @param data Data.frame or tibble with columns for emissions (numeric)
#' and sources (character or factor)
#' @import magrittr
#' @returns Data set (tibble) containing the emissions data for the best
#' performing source for New Source Performance Standards.
#' @export
MACT_NSPS=function(data){
  dat_means=data%>%dplyr::group_by(sources)%>%dplyr::summarize(means=mean(emissions))
  top_list=dat_means[order(dat_means$means,decreasing=F),]
  top_source=top_list$sources[1]
  dat_top=subset(data,data$sources==top_source)
  dat_top$sources=as.factor(dat_top$sources)
  dat_top$sources=droplevels(dat_top$sources)
  return(dat_top)
}
