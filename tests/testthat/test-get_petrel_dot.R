describe("test get_petrel_dot for open population", {
  resumen_anual <- data.frame(
    eventos = c(2),
    recapturas = c(50),
    capturas = c(102)
  )
  bobcat <- as.matrix(read_csv("/workdir/chicks.petrel/tests/data/bobcat.csv"))

  it("Markov Chain Monte Carlo", {
    set.seed(2)
    scenario <- "closed"
    obtained_petrel_dot <- get_petrel_dot(bobcat)
    obtained_mcmc_names <- names(summary(obtained_petrel_dot$mcmc)$quantiles[, 1])
    expected_mcmc_names <- c("pbeta[(Intercept)]", "N", "delta_1", "delta_2")
    expect_equal(obtained_mcmc_names, expected_mcmc_names)
    obtained_mcmc <- summary(obtained_petrel_dot$mcmc)
    obtained_N <- obtained_mcmc$quantiles["N", ]["50%"][[1]]
    expected_N <- 35
    expect_equal(obtained_N, expected_N)
  })
  it("Closed Time effects Markov Chain Monte Carlo ", {
    set.seed(2)
    scenario <- "closed_time"
    obtained_petrel_dot <- get_petrel_dot(bobcat, scenario)
    obtained_mcmc <- summary(obtained_petrel_dot$mcmc)
    obtained_N <- obtained_mcmc$quantiles["N", ]["50%"][[1]]
    expected_N <- 34
    expect_equal(obtained_N, expected_N)
  })
  it("Markov Chain Monte Carlo", {
    data <- as.matrix(read_csv("/workdir/chicks.petrel/tests/data/simdata_for_CJS.csv"))
    scenario <- "open"
    obtained_petrel_dot <- get_petrel_dot(data, scenario)
    obtained_mcmc <- names(summary(obtained_petrel_dot$mcmc)$quantiles[, 1])
    expected_mcmc <- c("pbeta[(Intercept)]", "phibeta[(Intercept)]")
    expect_equal(obtained_mcmc, expected_mcmc)
  })
})
