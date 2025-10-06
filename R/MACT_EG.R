#' Selects top sources from emissions data
#' @param CAA_section Applicable Clean Air Act section, either 112 or 129
#' @param data Data.frame or tibble with columns for emissions (numeric) and
#' sources (character or factor)
#' @returns Data set (tibble) of the top 5 or 12% of sources, depending on the
#' number of sources, to be used in UPL calculations for Maximum Achievable
#' Control Technology (MACT) floor analysis for Existing Source Guidelines.
#' @export

MACT_EG=function(CAA_section=112,data){
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
  n_sources=length(unique(data$sources))

  if (CAA_section==129){
    n_topsources=ceiling(0.12*n_sources)
    set.seed(1)
    source_index=sample.int(n_sources,n_topsources)
    dat_top=data[source_index,]

  } else if (CAA_section==112){
    # select top performers
    if (n_sources>=30){
      n_topsources=ceiling(0.12*n_sources)
      top_list=dat_means[order(dat_means$means,decreasing=F),]
      top_list=top_list$sources[1:n_topsources]
      dat_top=subset(data,data$sources%in%top_list)
    } else if (n_sources<30){
      n_topsources=5
      top_list=dat_means[order(dat_means$means,decreasing=F),]
      top_list=top_list$sources[1:n_topsources]
      dat_top=subset(data,data$sources%in%top_list)
    }
  }
  dat_topmeans=dplyr::summarize(dat_top,means=mean(emissions),.by='sources',
                                counts=dplyr::n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=forcats::fct_reorder(dat_topmeans$sources,
                                   dat_topmeans$means,.desc = FALSE)
  dat_top$sources=factor(dat_top$sources,levels=levels(dat_topmeans$sources))
  dat_topmeans=dplyr::arrange(dat_topmeans,means)
  dat_top=dplyr::arrange(dat_top,sources)
  dat_top$sources=droplevels(dat_top$sources)
  return(dat_top)
}
