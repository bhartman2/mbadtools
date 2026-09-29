test_that("gg_partial_residual_plots produces patchwork class", {
  data(freeny, package="datasets")
  fit = lm(y ~ ., data=freeny)
  expect_s3_class(gg_partial_residual_plots(fit),
                  "patchwork")
})

test_that("parsnip produces _lm class", {
  data(freeny, package="datasets")
  spec2 = parsnip::linear_reg() |>
    parsnip::set_engine("lm") |>
    parsnip::translate()
  fit2 = parsnip::fit(spec2, y ~ ., data = freeny)
  expect_s3_class(fit2, "_lm")
})

test_that("gg_partial_residual_plots produces patchwork object", {
  data(freeny, package="datasets")
  spec2 = parsnip::linear_reg() |>
    parsnip::set_engine("lm") |>
    parsnip::translate()
  fit2 = parsnip::fit(spec2, y ~ ., data = freeny)
  expect_s3_class(gg_partial_residual_plots(fit2, new_data=freeny), "patchwork")
})