#' Convert from a an emission fraction into a percent destruction or removal efficiency
#' @param x Numeric vector of emissions as a fraction out of 1
#' @returns Numeric vector of percent destruction or removal efficiencies
#' @export
DRE_trans=function(x){100*(1-x)}
