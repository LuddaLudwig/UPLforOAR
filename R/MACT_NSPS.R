#' Selects best performer from emissions dataset
#' @param dataset dataframe or tibble with columns for emissions (numeric) and sources (character or factor)
#' @import magrittr
#' @returns A tibble containing the emissions data for the best performing source
#' @export
MACT_NSPS=function(dataset){
  dat_means=dataset%>%dplyr::group_by(sources)%>%dplyr::summarize(means=mean(emissions))
  top_list=dat_means[order(dat_means$means,decreasing=F),]
  top_source=top_list$sources[1]
  dat_top=subset(dataset,dataset$sources==top_source)
  dat_top$sources=droplevels(dat_top$sources)
  return(dat_top)
}
