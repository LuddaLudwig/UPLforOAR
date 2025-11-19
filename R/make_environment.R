#' Makes a list of environments for automating analysis in reg text templates
#' @description
#' Takes a data set and variable lists to loop through and create a list of
#' self-contained environments that the child.Rmd documents can loop through for
#' the reg text document templates.
#' @export
#' @returns A list of environments with length equal to the number of UPL
#' calculations used in report
#' @param data A data set containing the sources, all emissions, and any
#' subcategories if applicable. Each row should be an individual run. Emissions
#' must be numeric, and sources and subcategories must be characters or factors.
#' @param HAP A string with the name of the Hazardous Air Pollutant (HAP) as it
#' should appear in text or figure labels.
#' @param emissions variable name or column number corresponding to the HAP
#' emissions used for selecting top performing sources.
#' @param sources variable name or column number corresponding to the source
#' designation, either a character or factor.
#' @param subcat_name The name of the subcategory variable corresponding to the HAP
#' given in `emissions`, either a character or factor. If there are no
#' subcategories for the HAP, use `NA`.
#' @param subcat_level If a subcategory is provided in `subcat_name`, indicate
#' which level here. For example, if the subcategory is `'facility_size'`, and
#' the levels are `c('small', 'medium', 'large')`, then one analysis might be
#' `subcat_level = 'medium'`. If there are no subcategories for the HAP, use `NA`.
#' @param type Either `'As-is'`, `'New'`, or `'Existing'`, Using `'As-is'` will
#' run the UPL analysis on the HAP emissions in `data` with no subsetting. `'New'`
#' will select the top performing source for the HAP using [MACT_new()], and
#' `'Existing'` will select the top performing sources for the HAP using [MACT_existing()].
#' @param meas_unit String of measurment units to be used in text and figure axes labels.
#' @param CAA_section Applicable Clean Air Act section, either 112 or 129.
#' @param national_N For Clean Air Act section 129 (`CAA_section = 129`), the
#' additional argument providing the total number of sources nation-wide
#' regardless of whether or not they contributed emissions data is required.
#' @param future_runs Integer of future runs to use in prediction, the default
#' is `3` since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is `0.99`.
#' @param ... Other arguments passed on to [Bayesian_UPL()].
make_environment = function(data, HAP, emissions, sources, subcat_name = NA,
                            subcat_level = NA, type = c('New', 'Existing', 'As-is'),
                            CAA_section = 112, national_N = NA, meas_unit,
                            future_runs = 3, significance = 0.99, ...){
  future_runs = as.integer(future_runs)
  if (!is.integer(future_runs)){
    stop("future_runs must be a positive integer")
  }
  if (future_runs < 1){
    stop("future_runs must be a positive integer")
  }
  if (significance >= 1){
    stop("significance must be greater then 0 and less than 1")
  }
  if (significance <= 0){
    stop("significance must be greater then 0 and less than 1")
  }
  if (is.na(subcat_name)){
    data_temp = tibble::tibble(emissions = data[[emissions]],
                               sources = data[[sources]])
  } else {
    data_temp = tibble::tibble(emissions = data[[emissions]],
                               sources = data[[sources]],
                               subcat = data[[subcat_name]])
    if (!is.factor(data_temp$subcat)){
      data_temp$subcat = factor(data_temp$subcat)
    }
    if (!(subcat_level %in% levels(data_temp$subcat))){
      stop("subcat_level does not match levels of subcat_name")
    }
    data_temp = subset(data_temp, data_temp$subcat == subcat_level)
  }
  if (!is.numeric(data_temp$emissions)){
    stop("Emissions must be numeric vector")
  }
  if ((!is.character(data_temp$sources)) & (!is.factor(data_temp$sources))){
    stop("Sources must be a character or factor vector")
  }
  if (!is.character(emissions)){
    emissions = colnames(data[emissions])
  }
  if (!is.character(sources)){
    sources = colnames(data[sources])
  }
  analysis_env = list2env(list(HAP = HAP, emissions = emissions,
                               subcat_name =  subcat_name,
                               subcat_level = subcat_level,
                               data = data_temp, CAA_section = CAA_section,
                               national_N = national_N,
                               meas_unit = meas_unit,
                               sources = sources, type = type,
                               significance = significance,
                               future_runs = future_runs,
                               more_args = ...),
                          parent = as.environment(2))
  return(analysis_env)
}
