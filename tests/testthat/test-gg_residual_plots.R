data(freeny, package="datasets")
fit = lm(y ~ ., data=freeny)
# for testing response argument in the model item 8
freeny1 = freeny %>% rename(Sales=y)
fit1 = lm(Sales ~ ., data=freeny1)
afit1 = augment(fit1)
expect_true("Sales" %in% names(fit1$model))

spec2 = parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::translate()
fit2 = parsnip::fit(spec2, Sales ~ ., data=freeny1)

test_that("parsnip produces _lm class", {
  expect_s3_class(fit2, "_lm")
})
  
test_that("gg_residual_plots produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit), "patchwork")
  expect_s3_class(gg_residual_plots(fit2$fit, new_data=freeny1), "patchwork")
})

test_that("gg_residual_plots item=8 produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit, items=8), "patchwork")
  expect_s3_class(gg_residual_plots(fit1, new_data=freeny1, items=8), "patchwork")
})

test_that("gg_residual_plots item=10 produces patchwork class", {
  expect_s3_class(gg_residual_plots(fit, items=10),
   "patchwork")
  expect_s3_class(gg_residual_plots(fit1, new_data=freeny1, items=10), 
   "patchwork")
})

test_that("Tukey_test produces a gt table object", {
  # fit is linear class 'lm'
  fit = lm(mpg~hp, data=mtcars)
  S = Tukey_test(fit, fmt = "summary")
  G = Tukey_test(fit)
  expect_true("Variable" %in% names(S))
  expect_true("Test.stat" %in% names(S))
  expect_true("p.value" %in% names(S))
  expect_s3_class(G, "gt_tbl")
})

test_that("Tukey_test produces a message", {
  # fit1 is not linear 'quadratic or interaction'
  fit1 = lm(mpg ~ hp:disp, data=mtcars)
  S = Tukey_test(fit1, fmt = "summary")
  G = Tukey_test(fit1)
  expect_s3_class(S, "data.frame")
  expect_s3_class(G, "gt_tbl")
})