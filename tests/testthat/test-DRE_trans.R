test_that("DRE_trans() convert fraction emitted to percent removal", {
  x_vector=c(0.01,0.2,0.0001)
  DRE_trans(x_vector)
  expect_equal(DRE_trans(x_vector),c(99,80,99.99))
})
