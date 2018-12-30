test_sim <- function() {
  mySim <- load_safe("sim.RDS")

  no_error_in_expr({ extract_ysim(mySim) })
  no_error_in_expr({ extract_zsim(mySim) })

  invisible()
}

test_fit <- function() {
  myFit <- load_safe("draw_samples.RDS")

  no_error_in_expr({ print(myFit) })
  no_error_in_expr({ show(myFit) })
  no_error_in_expr({ extract_alpha(myFit) })
  no_error_in_expr({ extract_data(myFit) })
  no_error_in_expr({ extract_diagnostics(myFit) })
  no_error_in_expr({ extract_filename(myFit) })
  no_error_in_expr({ extract_gamma(myFit) })
  no_error_in_expr({ extract_K(myFit) })
  no_error_in_expr({ extract_n_chains(myFit) })
  no_error_in_expr({ extract_model(myFit) })
  no_error_in_expr({ extract_obs_parameters(myFit) })
  no_error_in_expr({ extract_parameter_names(myFit) })
  no_error_in_expr({ extract_parameters(myFit) })
  no_error_in_expr({ extract_quantity(myFit, par = "A") })
  no_error_in_expr({ extract_R(myFit) })
  no_error_in_expr({ extract_seed(myFit) })
  no_error_in_expr({ extract_spec(myFit) })
  no_error_in_expr({ extract_T(myFit) })
  no_error_in_expr({ extract_time(myFit) })
  no_error_in_expr({ extract_y(myFit) })
  no_error_in_expr({ extract_ypred(myFit) })
  no_error_in_expr({ extract_ysim(myFit) })
  no_error_in_expr({ extract_zpred(myFit) })
  no_error_in_expr({ extract_zsim(myFit) })
  no_error_in_expr({ extract_zstar(myFit) })

  invisible()
}

test_plots <- function() {
  myFit <- load_safe("draw_samples.RDS")

  no_error_in_expr({ plot_series(myFit) })
  no_error_in_expr({ plot_state_probability(myFit) })
  no_error_in_expr({
    plot_ppredictive(
      myFit,
      type = c("density", "cumulative", "boxplot",
               "histogram", "scatterplot", "ks"),
      fun = mean, fun1 = mean, fun2 = mean
    )
  })

  invisible()
}

test_optimizing <- function() {
  myOpt <- load_safe("optimizing_all.RDS")
  no_error_in_expr({ extract_grid(myOpt, pars = "mu") })
  no_error_in_expr({ extract_best(myOpt) })

  myOpt <- extract_best(myOpt)
  no_error_in_expr({ extract_data(myOpt) })
  no_error_in_expr({ extract_filename(myOpt) })
  no_error_in_expr({ extract_gamma(myOpt) })
  no_error_in_expr({ extract_K(myOpt) })
  no_error_in_expr({ extract_model(myOpt) })
  no_error_in_expr({ extract_obs_parameters(myOpt) })
  no_error_in_expr({ extract_parameter_names(myOpt) })
  no_error_in_expr({ extract_parameters(myOpt) })
  no_error_in_expr({ extract_quantity(myOpt, par = "A") })
  no_error_in_expr({ extract_R(myOpt) })
  no_error_in_expr({ extract_seed(myOpt) })
  no_error_in_expr({ extract_spec(myOpt) })
  no_error_in_expr({ extract_T(myOpt) })
  no_error_in_expr({ extract_time(myOpt) })
  no_error_in_expr({ extract_y(myOpt) })
  no_error_in_expr({ extract_ypred(myOpt) })
  no_error_in_expr({ extract_ysim(myOpt) })
  no_error_in_expr({ extract_zpred(myOpt) })
  no_error_in_expr({ extract_zsim(myOpt) })
  no_error_in_expr({ extract_zstar(myOpt) })

  invisible()
}

testExtractors <- function() {
  myFit <- load_safe("draw_samples.RDS")

  no_error_in_expr({ extract_alpha(myFit, reduce = NULL, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_alpha(myFit, reduce = NULL, combine = NULL, chain = 3) })
  no_error_in_expr({ extract_alpha(myFit, reduce = mean, combine = NULL, chain = 1) })
  no_error_in_expr({ extract_alpha(myFit, reduce = mean, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_alpha(myFit, reduce = mean, combine = c, chain = 1) })
  no_error_in_expr({ extract_alpha(myFit, chain = "all") })
  no_error_in_expr({ extract_alpha(myFit) })

  no_error_in_expr({ classify_alpha(myFit, reduce = NULL, chain = "all") })
  no_error_in_expr({ classify_alpha(myFit, reduce = median, chain = "all") })
  no_error_in_expr({ classify_alpha(myFit, reduce = median, chain = 1) })
  no_error_in_expr({ classify_alpha(myFit, reduce = mean, chain = 1) })
  no_error_in_expr({ classify_alpha(myFit, chain = 1) })
  no_error_in_expr({ classify_alpha(myFit) })

  no_error_in_expr({ extract_gamma(myFit, reduce = NULL, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_gamma(myFit, reduce = NULL, combine = NULL, chain = 3) })
  no_error_in_expr({ extract_gamma(myFit, reduce = mean, combine = NULL, chain = 1) })
  no_error_in_expr({ extract_gamma(myFit, reduce = mean, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_gamma(myFit, reduce = mean, combine = c, chain = 1) })
  no_error_in_expr({ extract_gamma(myFit, chain = "all") })
  no_error_in_expr({ extract_gamma(myFit) })

  no_error_in_expr({ classify_gamma(myFit, reduce = NULL, chain = "all") })
  no_error_in_expr({ classify_gamma(myFit, reduce = median, chain = "all") })
  no_error_in_expr({ classify_gamma(myFit, reduce = median, chain = 1) })
  no_error_in_expr({ classify_gamma(myFit, reduce = mean, chain = 1) })
  no_error_in_expr({ classify_gamma(myFit, chain = 1) })
  no_error_in_expr({ classify_gamma(myFit) })

  no_error_in_expr({ extract_zstar(myFit, reduce = NULL, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_zstar(myFit, reduce = NULL, combine = NULL, chain = 3) })
  no_error_in_expr({ extract_zstar(myFit, reduce = mean, combine = NULL, chain = 1) })
  no_error_in_expr({ extract_zstar(myFit, reduce = mean, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_zstar(myFit, reduce = mean, combine = c, chain = 1) })
  no_error_in_expr({ extract_zstar(myFit, chain = "all") })
  no_error_in_expr({ extract_zstar(myFit) })

  no_error_in_expr({ classify_zstar(myFit, reduce = NULL, chain = "all") })
  no_error_in_expr({ classify_zstar(myFit, reduce = median, chain = "all") })
  no_error_in_expr({ classify_zstar(myFit, reduce = median, chain = 1) })
  no_error_in_expr({ classify_zstar(myFit, reduce = mean, chain = 1) })
  no_error_in_expr({ classify_zstar(myFit, chain = 1) })
  no_error_in_expr({ classify_zstar(myFit) })

  no_error_in_expr({ extract_ypred(myFit, reduce = NULL, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_ypred(myFit, reduce = NULL, combine = NULL, chain = 3) })
  no_error_in_expr({ extract_ypred(myFit, reduce = mean, combine = NULL, chain = 1) })
  no_error_in_expr({ extract_ypred(myFit, reduce = mean, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_ypred(myFit, reduce = mean, combine = c, chain = 1) })
  no_error_in_expr({ extract_ypred(myFit, chain = "all") })
  no_error_in_expr({ extract_ypred(myFit) })

  no_error_in_expr({ extract_zpred(myFit, reduce = NULL, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_zpred(myFit, reduce = NULL, combine = NULL, chain = 3) })
  no_error_in_expr({ extract_zpred(myFit, reduce = mean, combine = NULL, chain = 1) })
  no_error_in_expr({ extract_zpred(myFit, reduce = mean, combine = NULL, chain = "all") })
  no_error_in_expr({ extract_zpred(myFit, reduce = mean, combine = c, chain = 1) })
  no_error_in_expr({ extract_zpred(myFit, chain = "all") })
  no_error_in_expr({ extract_zpred(myFit) })

  no_error_in_expr({ extract_quantity(myFit, par = "mu*", reduce = median, combine = cbind, chain = "all") })
  no_error_in_expr({ extract_quantity(myFit, par = "mu*", reduce = mean, combine = c, chain = 1) })

  invisible()
}
