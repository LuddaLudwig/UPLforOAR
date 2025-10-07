test_that("Skewed_UPL() calculates UPL assuming emissions data are skew-normal", {
  top5=tibble::tibble(emissions=c(1,2,1.5,
                          1.2,3,2.2,
                          0.2,0.4,0.7,
                          0.1,0.2,0.3,1.1,1.2,1.3,
                          0.1,0.1,0.15,0.16,0.19,1.8),
              sources=c(rep('A',3),
                        rep('B',3),
                        rep('C',3),
                        rep('D',6),
                        rep('E',6)))
  dat_topmeans=dplyr::summarize(top5,means=mean(emissions),.by='sources',
                                counts=dplyr::n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=forcats::fct_reorder(dat_topmeans$sources,
                                   dat_topmeans$means,.desc = FALSE)
  top5$sources=factor(top5$sources,levels=levels(dat_topmeans$sources))
  top5=dplyr::arrange(top5,sources)
  test_result=Skewed_UPL(data=top5)
  expect_equal(test_result,2.50184469)

  small_dat=tibble::tibble(emissions=c(4.2,5.1,6.7),
                           sources=c("A","A","A"))
  test_result2=tryCatch({
    result <- Skewed_UPL(data=small_dat)
    print(result)
  }, error = function(e) {
      output=(e$message)
    return(output)
  })
  expect_equal(test_result2,NA)
})
