MACT_EG=function(CAA_section=112,dataset){

  dat_means=dataset%>%group_by(sources)%>%summarize(means=mean(emissions))
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
  return(dat_top)
}
