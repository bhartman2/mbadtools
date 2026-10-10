data(longley, package="datasets")
# this data is known to be collinear

test_that("Kleinsrule works", {
  fm1 <- stats::lm(Employed ~ ., data = longley)
  Z = Kleinsrule(longley,  response = "Employed")
  expect_s3_class(Z, "tbl_df")
  expect_equal(summary(fm1)$r.squared, 
               as.numeric(Z[1,"rsq"]))
})
