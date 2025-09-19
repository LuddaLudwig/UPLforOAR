#' Selects top sources from emissions dataset
#' @param CAA_section applicable Clean Air Act section, either 112 or 129
#' @param dataset dataframe or tibble with columns for emissions (numeric) and sources (character or factor)
#' @import magrittr
#' @return a tibble of the top sources to be used in UPL calculations
#' @export

MACT_EG=function(CAA_section=112,dataset){

  dat_means=dataset%>%dplyr::group_by(sources)%>%dplyr::summarize(means=mean(emissions))
  n_sources=length(unique(dataset$sources))

  if (CAA_section==129){
    n_topsources=ceiling(0.12*n_sources)
    set.seed(1)
    source_index=sample.int(n_sources,n_topsources)
    dat_top=dataset[source_index,]

  } else if (CAA_section==112){
    # select top performers
    if (n_sources>=30){
      n_topsources=ceiling(0.12*n_sources)
      top_list=dat_means[order(dat_means$means,decreasing=F),]
      top_list=top_list$sources[1:n_topsources]
      dat_top=subset(dataset,dataset$sources%in%top_list)
    } else if (n_sources<30){
      n_topsources=5
      top_list=dat_means[order(dat_means$means,decreasing=F),]
      top_list=top_list$sources[1:n_topsources]
      dat_top=subset(dataset,dataset$sources%in%top_list)
    }
  }
  dat_topmeans=dat_top%>%group_by(sources)%>%summarize(means=mean(emissions),
                                                                   counts=n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=fct_reorder(dat_topmeans$sources,
                                   dat_topmeans$means,.desc = FALSE)
  dat_top$sources=factor(dat_top$sources,levels=levels(dat_topmeans$sources))
  dat_topmeans=arrange(dat_topmeans,means)
  dat_top=arrange(dat_top,sources)
  return(dat_top)
}
