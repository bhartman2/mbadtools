test_that("gt_add_significance produces a gt_tbl object of correct length", {
  
  data(cars)
  fit = lm(dist~speed, data=cars);
  expect_s3_class(gt_add_significance(broom::tidy(fit)), "gt_tbl")
  expect_length(gt_add_significance(broom::tidy(fit)), 20)
  
})
