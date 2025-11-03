#' Selects best performer from emissions data
#' @param data Data.frame or tibble with columns for `emissions` (numeric)
#' and `sources` (character or factor)
#' @returns Data set (tibble) containing the emissions data for the best
#' performing source for New Source Performance Standards.
#' @export
MACT_new = function(data){
  if (("emissions" %in% names(data)) == FALSE){
    stop("data must have numeric column named 'emissions' ")
  }
  if (("sources" %in% names(data)) == FALSE){
    stop("data must have character or factor column named 'sources' ")
  }
  if (!is.numeric(data$emissions)){
    stop("Emissions must be numeric vector")
  }
  if ((!is.character(data$sources)) & (!is.factor(data$sources))){
    stop("Sources must be a character or factor vector")
  }
  if (any(data$emissions < 0)){
    warning("emissions data contain negative values")
  }
  if (any(data$emissions == 0)){
    warning("emissions data contain zero values and have been removed")
    data = subset(data, data$emissions != 0)
  }
  dat_means = dplyr::summarize(data, means = mean(emissions), .by = 'sources')
  top_list = dat_means[order(dat_means$means, decreasing = F), ]
  top_source = top_list$sources[1]
  dat_top = subset(data, data$sources == top_source)
  dat_top$sources = as.factor(dat_top$sources)
  dat_top$sources = droplevels(dat_top$sources)
  return(dat_top)
}
