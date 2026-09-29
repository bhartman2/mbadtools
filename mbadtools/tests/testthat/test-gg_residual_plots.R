data(freeny, package="datasets")
fit = lm(y ~ ., data=freeny)
# for testing response argument in the model item 8
freeny1 = freeny %>% rename(Sales=y)
fit1 = lm(Sales ~ ., data=freeny1)
afit1 = augment(fit1)
nm = colnames(fit1$model)[1]
expect_match(nm,"Sales")
response = afit1 %>% pull(nm)
expect_length(response, nrow(afit1))

spec2 = parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::translate()
fit2 = parsnip::fit(spec2, Sales ~ ., data=freeny1)

test_that("parsnip produces _lm class", {
  expect_s3_class(fit2, "_lm")
})
  
test_that("gg_residual_plots produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit), "patchwork")
  expect_s3_class(gg_residual_plots(fit2, new_data=freeny1), "patchwork")
})

test_that("gg_residual_plots item=8 produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit, items=8), "patchwork")
  expect_s3_class(gg_residual_plots(fit1, new_data=freeny1, items=8), "patchwork")
})

test_that("gg_residual_plots item=8 produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit2, new_data=freeny1), "patchwork")
})

test_that("gg_residual_plots item=8 produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit2, new_data=freeny1, items=8), "patchwork")
})