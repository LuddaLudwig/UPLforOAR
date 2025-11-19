#' Selects top sources from emissions data
#' @param CAA_section Applicable Clean Air Act section, either 112 or 129
#' @param data Data.frame or tibble with columns for `emissions` (numeric) and
#' `sources` (character or factor).
#' @param emissions variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @param sources variable name or column number corresponding to the source
#' designation, either a character or factor.
#' @returns Data set (tibble) of the top 5 or 12 percent of sources, depending on the
#' number of sources, to be used in UPL calculations for Maximum Achievable
#' Control Technology (MACT) floor analysis for Existing Source Guidelines.
#' @param national_N For Clean Air Act section 129 (`CAA_section = 129`), the
#' additional argument providing the total number of sources nation-wide
#' regardless of whether or not they contributed emissions data is required.
#' @description
#' Ranks the sources by their average emission from best to worst, then selects
#' either the top 5 or top 12 percent depending on the applicable CAA section
#' and number of sources with data available.
#' @export
MACT_existing = function(data, emissions, sources, CAA_section = 112,
                         national_N = NULL){
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
  n_sources = length(unique(data_temp$sources))
  if (CAA_section == 129){
    if (is.null(national_N)){
      stop("Must provide total number of sources as national_N")
    }
    n_topsources = ceiling(0.12 * national_N)
    if (n_sources < n_topsources){
      n_topsources = n_sources
    }
    top_list = dat_means[order(dat_means$means, decreasing = F), ]
    top_list = top_list$sources[1:n_topsources]
    dat_top = subset(data_temp, data_temp$sources%in%top_list)
  } else if (CAA_section == 112){
    if (n_sources >= 30){
      n_topsources = ceiling(0.12 * n_sources)
      top_list = dat_means[order(dat_means$means, decreasing = F), ]
      top_list = top_list$sources[1:n_topsources]
      dat_top = subset(data_temp, data_temp$sources%in%top_list)
    } else if (n_sources < 30){
      n_topsources = 5
      top_list = dat_means[order(dat_means$means, decreasing = F), ]
      top_list = top_list$sources[1:n_topsources]
      dat_top = subset(data_temp, data_temp$sources%in%top_list)
    }
  }
  dat_topmeans = dplyr::summarize(dat_top ,means = mean(emissions),
                                  .by='sources', counts=dplyr::n())
  dat_topmeans$sources = as.factor(dat_topmeans$sources)
  dat_topmeans$sources = forcats::fct_reorder(dat_topmeans$sources,
                                   dat_topmeans$means, .desc = FALSE)
  dat_top$sources = factor(dat_top$sources,
                           levels = levels(dat_topmeans$sources))
  dat_topmeans = dplyr::arrange(dat_topmeans, means)
  dat_top = dplyr::arrange(dat_top, sources)
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
