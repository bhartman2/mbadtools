data(chickwts, package="datasets")
K = chickwts %>% group_by(feed) %>% 
  summarize(avwt = mean(weight))

test_that("pareto data structure works", {
  K1 = pareto_arrange_data(K, avwt)
  nm = names(K1)  
  expect_s3_class(K1, "data.frame")
  expect_setequal(nm, 
                  c('feed','avwt','Cumulative','Percentage'))
})

test_that("pareto data plot creates a ggplot", {
  K1 = pareto_arrange_data(K, avwt)
  P = pareto_plot(K1, feed, avwt) +
    pareto_sec_axis(max(K1$avwt)) +
    pareto_pct_labels(max(K1$avwt), nudge_y=1, size=3) +
    pareto_cutoff_line(max(K1$avwt))
  expect_length(P,1)
  expect_s3_class(P, "ggplot")
})