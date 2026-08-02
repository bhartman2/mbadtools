test_that("GrangerTable produces data frame with correct colnames", {
  data(moody, package="mbadtools")
  T = GrangerTable(moody, ICS, Wgrowth)
  expect_s3_class(T,
    "data.frame")
  expect_setequal(names(T),
               c('lag', 'ICS =G=> Wgrowth', 'Wgrowth =G=> ICS'))
})

test_that("GrangerPlot works", {
  data(moody, package="mbadtools")
  G = GrangerPlot(GrangerTable(moody, ICS, Wgrowth))
  expect_length(G,2)
  expect_s3_class(G[[1]], "ggplot")
})
