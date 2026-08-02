test_that("gg_residual_plots produces patchwork class", {
  data(freeny, package="datasets")
  fit = lm(y ~ ., data=freeny)
  expect_s3_class(gg_residual_plots(fit), "patchwork")
})

test_that("gg_residual_plots item=8 produces patchwork class", {
  data(freeny, package="datasets")
  fit = lm(y ~ ., data=freeny)
  expect_s3_class(gg_residual_plots(fit, items=8), "patchwork")
})