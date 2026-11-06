nb <- voi::chemo_nb
pars <- voi::chemo_pars
cea <- voi::chemo_cea
p1 <- "p_side_effects_t1"
p2 <- c("p_side_effects_t1", "p_side_effects_t2")

test_that("EVPI matches voi", {
  local_service()
  same_result(evpi_remote(nb), voi::evpi(nb))
  same_result(evpi_remote(cea, nsim = 1000), voi::evpi(cea, nsim = 1000))
})

test_that("EVPPI matches voi for each deterministic method", {
  local_service()
  same_result(evppi_remote(nb, pars, pars = p1), voi::evppi(nb, pars, pars = p1))
  same_result(evppi_remote(nb, pars, pars = list(p1, p2)),
              voi::evppi(nb, pars, pars = list(p1, p2)))
  same_result(evppi_remote(cea, pars, pars = p2, nsim = 500),
              voi::evppi(cea, pars, pars = p2, nsim = 500))
  same_result(evppi_remote(nb, pars, pars = p2, method = "gp", nsim = 300),
              voi::evppi(nb, pars, pars = p2, method = "gp", nsim = 300))
  same_result(evppi_remote(nb, pars, pars = p2, method = "earth"),
              voi::evppi(nb, pars, pars = p2, method = "earth"))
  same_result(evppi_remote(nb, pars, pars = p1, method = "so", n.blocks = 20),
              voi::evppi(nb, pars, pars = p1, method = "so", n.blocks = 20))
  same_result(evppi_remote(nb, pars, pars = p1, method = "sal"),
              voi::evppi(nb, pars, pars = p1, method = "sal"))
})

test_that("random methods match voi with the same seed", {
  local_service()
  r <- evppi_remote(nb, pars, pars = p1, se = TRUE, B = 50,
                    .remote = remote_options(seed = 11))
  set.seed(11)
  same_result(r, voi::evppi(nb, pars, pars = p1, se = TRUE, B = 50))
  r <- evppi_remote(nb, pars, pars = p2, method = "bart", nsim = 300,
                    .remote = remote_options(seed = 12))
  set.seed(12)
  same_result(r, voi::evppi(nb, pars, pars = p2, method = "bart", nsim = 300))
  # Without a seed, one is drawn here, so set.seed() makes runs repeatable.
  set.seed(5); a <- evppi_remote(nb, pars, pars = p1, se = TRUE, B = 20)
  set.seed(5); b <- evppi_remote(nb, pars, pars = p1, se = TRUE, B = 20)
  expect_identical(a, b)
})

test_that("check = TRUE returns the fitted models", {
  local_service()
  r <- evppi_remote(nb, pars, pars = p1, check = TRUE)
  l <- voi::evppi(nb, pars, pars = p1, check = TRUE)
  expect_identical(r$evppi, l$evppi)
  expect_identical(r$pars, l$pars)
  expect_s3_class(attr(r, "models")[[1]][[1]], "gam")
  expect_equal(fitted(attr(r, "models")[[1]][[1]]), fitted(attr(l, "models")[[1]][[1]]),
               tolerance = 0)
})

test_that("EVPPI of a variance matches voi", {
  local_service()
  y <- pars$p_side_effects_t1 * 10 + rnorm(nrow(pars))
  same_result(evppivar_remote(y, pars, pars = p1, verbose = FALSE),
              voi::evppivar(y, pars, pars = p1, verbose = FALSE))
})

test_that("EVSI, ENBS and population VoI match voi", {
  local_service()
  # EVSI simulates study data, so both sides use the same seed.
  e <- evsi_remote(nb, pars, study = "trial_binary", pars = p2, n = c(100, 500),
                   .remote = remote_options(seed = 2))
  set.seed(2)
  el <- voi::evsi(nb, pars, study = "trial_binary", pars = p2, n = c(100, 500))
  same_result(e, el)
  r <- evsi_remote(nb, pars, study = "trial_binary", pars = p2, n = 100, method = "is",
                   nsim = 300, .remote = remote_options(seed = 3))
  set.seed(3)
  same_result(r, voi::evsi(nb, pars, study = "trial_binary", pars = p2, n = 100,
                           method = "is", nsim = 300))
  # voi's own ENBS test case (tests/testthat/test_enbs.R): first ENBS -9,250,000.
  en <- enbs_remote(voi::chemo_evsi_or, costs_setup = c(5e6, 1e7),
                    costs_pp = c(28000, 42000), pop = 46000, time = 10)
  expect_equal(en$enbs[1], -9250000)
  enl <- voi::enbs(voi::chemo_evsi_or, costs_setup = c(5e6, 1e7),
                   costs_pp = c(28000, 42000), pop = 46000, time = 10)
  same_result(en, enl)
  same_result(enbs_opt_remote(enl), voi::enbs_opt(enl))
  same_result(pop_voi_remote(el$evsi, pop = 1e4, time = 10),
              voi::pop_voi(el$evsi, pop = 1e4, time = 10))
})

test_that("voi errors and warnings come back", {
  local_service()
  expect_error(evppi_remote(nb, pars, pars = "nonexistent"),
               "not found in columns of `inputs`")
  expect_error(evsi_remote(nb, pars, datagen_fn = function(inputs) inputs, pars = p1),
               "Only data can be sent")
})

test_that("results plot with voi", {
  local_service()
  r <- evppi_remote(nb, pars, pars = list(p1, p2))
  expect_s3_class(r, "evppi")
  expect_s3_class(plot(r), "ggplot")
})

test_that("the service reports its versions", {
  local_service()
  i <- voi_remote_info()
  expect_equal(i$versions$voi, as.character(packageVersion("voi")))
})
