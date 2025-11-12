#' Selects best performer from emissions data
#' @param data Data.frame or tibble with columns for `emissions` (numeric)
#' and `sources` (character or factor)
#' @param emissions variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @param sources variable name or column number corresponding to the source
#' designation, either a character or factor.
#' @returns Data set (tibble) containing the emissions data for the best
#' performing source for New Source Performance Standards.
#' @export
MACT_new = function(data, emissions, sources){
  data_temp = tibble::tibble(emissions = data[[emissions]],
                             sources = data[[sources]])
  if (!is.numeric(data_temp$emissions)){
    stop("Emissions must be numeric vector")
  }
  if ((!is.character(data_temp$sources)) & (!is.factor(data_temp$sources))){
    stop("Sources must be a character or factor vector")
  }
  if (any(data_temp$emissions < 0)){
    warning("emissions data contain negative values")
  }
  if (any(data_temp$emissions == 0)){
    warning("emissions data contain zero values and have been removed")
    data_temp = subset(data_temp, data_temp$emissions != 0)
  }
  dat_means = dplyr::summarize(data_temp, means = mean(emissions),
                               .by = 'sources')
  top_list = dat_means[order(dat_means$means, decreasing = F), ]
  top_source = top_list$sources[1]
  dat_top = subset(data_temp, data_temp$sources == top_source)
  dat_top$sources = as.factor(dat_top$sources)
  dat_top$sources = droplevels(dat_top$sources)
  if (!is.character(emissions)){
    emissions = colnames(data)[emissions]
  }
  if (!is.character(sources)){
    sources = colnames(data)[sources]
  }
  names(dat_top) = c(emissions, sources)
  return(dat_top)
}
