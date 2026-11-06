test_that("requests follow the ModelsCloud v2 protocol and async results are polled", {
  skip_if_not_installed("voipexa")
  sent <- list()
  fake_post <- function(url, body, token) {
    sent[[length(sent) + 1]] <<- list(url = url, body = body, token = token)
    if (grepl("/call/v2/", url)) {
      return(list(result = "running", executionId = "x1"))
    }
    fi <- sent[[1]]$body$funcInput
    out <- voipexa::gateway(fn = fi$fn, args_json = fi$args_json, seed = fi$seed)
    list(result = "succeeded", executionId = "x1",
         output = jsonlite::fromJSON(out, simplifyVector = FALSE))
  }
  local_mocked_bindings(.vr_post = fake_post)
  r <- evpi_remote(voi::chemo_nb, .remote = remote_options(token = "t", poll = 0))
  expect_equal(r, voi::evpi(voi::chemo_nb), tolerance = 0)
  expect_equal(sent[[1]]$url, "https://core.modelscloud.resp.core.ubc.ca/call/v2/resplab/voipexa")
  expect_equal(sent[[1]]$body$processMode, "Async")
  expect_true(sent[[1]]$body$funcInput$async)
  expect_equal(sent[[1]]$body$funcInput$fn, "evpi")
  expect_equal(sent[[2]]$url, "https://core.modelscloud.resp.core.ubc.ca/execution/getResult")
  expect_equal(sent[[2]]$body$executionId, "x1")
})

test_that("a missing Access Key is reported", {
  expect_error(evpi_remote(voi::chemo_nb, .remote = remote_options(token = "")),
               "MODELSCLOUD_TOKEN")
})

test_that("EVPPI sends only the analysed parameter columns", {
  a <- .vr_trim_inputs(list(inputs = voi::chemo_pars, pars = list("p_side_effects_t1",
                                                                  c("p_side_effects_t1", "c_home_care"))))
  expect_equal(colnames(a$inputs), c("p_side_effects_t1", "c_home_care"))
})
