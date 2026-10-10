test_that("gg_vip works", {
  mpg_fit = lm(mpg ~ ., data=mtcars)
  V = vip::vi(mpg_fit)
  P = gg_vip(V)
  expect_true("Variable" %in% names(V))
  expect_true("Importance" %in% names(V))
  expect_s3_class(P, "ggplot")
})

test_that("gg_pdp works", {
  skip_if_not_installed("randomForest")
  rf_model = randomForest::randomForest(mpg ~ disp + hp, data=mtcars, ntree=50)
  Pd = pdp::partial(rf_model, pred.var="disp", train=mtcars,
               type="regression")
  expect_true("disp" %in% names(Pd))
  expect_true("yhat" %in% names(Pd))
  expect_s3_class(Pd, "data.frame")
  P = gg_pdp(Pd)
  expect_s3_class(P, "ggplot")
})
