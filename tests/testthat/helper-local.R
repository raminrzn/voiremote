# Runs the service package in this session instead of on ModelsCloud, and
# passes its output back as JSON text, as the platform does.
local_transport <- function(input, opts) {
  voipexa::gateway(fn = input$fn, args_json = input$args_json, seed = input$seed)
}

local_service <- function(env = parent.frame()) {
  skip_if_not_installed("voipexa")
  withr::local_options(list(voiremote.transport = local_transport), .local_envir = env)
}

# Same values and attributes; fitted models attached by check = TRUE carry
# environments, which differ by reference after a round trip.
same_result <- function(remote, local) {
  expect_equal(remote, local, tolerance = 0)
}
