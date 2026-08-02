test_that("gg_partial_residual_plots produces patchwork class", {
  data(freeny, package="datasets")
  fit = lm(y ~ ., data=freeny)
  expect_s3_class(gg_partial_residual_plots(fit),
                  "patchwork")
})