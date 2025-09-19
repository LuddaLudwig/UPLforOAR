#' Convert from a an emission fraction into a percent destruction or removal efficiency
#' @param x numeric vector of emissions as a fraction out of 1
#' @returns numeric vector of percent destruction or removal efficiencies
DRE_trans=function(x){100*(1-x)}
