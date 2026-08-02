# setup
  require(vars)
  require(bvartools)
  
  data("Canada", package="vars")
  var.2c <- vars::VAR(Canada, p = 2, type = "const")
  
  data("e1", package="bvartools")
  e1 <- diff(log(e1)) * 100
  # Generate model data
  model <- bvartools::gen_var(e1, p = 2, deterministic = 2,
                            iterations = 100, burnin = 10)
  # Number of iterations and burnin should be much higher.
  # Add prior specifications
  model <- bvartools::add_priors(model)
  # Obtain posterior draws
  res=capture.output(
    object <- suppressMessages(bvartools::draw_posterior(model))
  )

test_that("setup works",{
  expect_s3_class(get("var.2c"), "varest")
  expect_s3_class(get("object"), 'bvar')
})

test_that("irf plot for vars works", {
  IRF = vars::irf(var.2c, impulse = "e", response = "prod")
  expect_s3_class(ggvar_irf(IRF), "ggplot")
})

test_that("bvarirf_to_varirf converts class properly", {
  # Calculate IR
  ir <- bvartools::irf(object, impulse = "invest", response = "cons")
  # convert
  X = bvarirf_to_varirf(ir, impulse="invest", response="cons")
  expect_s3_class(X, "varirf")
})

test_that("fevd plot works", {
  P = ggvar_fevdplot(object, type="oir", n.ahead=7)
  expect_length(P,3)
  expect_s3_class(P[[1]], "ggplot")
})

test_that("bvartools forecast plot works", {
  # Calculate forecasts
  pred <- predict(object, new_d = rep(1, 10))
  # Plot forecasts
  P = ggvar_forecastplot(pred)
  expect_length(P,3)
  expect_s3_class(P[[1]], "ggplot")
})

test_that("vars stability plot works", {
  stab = vars::stability(var.2c)
   # override defaults for generic plot
  P = ggvar_plot_stability(stab, lines=FALSE, breaks=FALSE)
  expect_length(P,4)
  expect_s3_class(P[[1]], "ggplot")
  
})