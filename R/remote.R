#' Options for running voi on ModelsCloud
#'
#' @param token Your ModelsCloud Access Key. By default it is read from the
#'   environment variable `MODELSCLOUD_TOKEN` (for example, set in
#'   `~/.Renviron`). Keys are personal; do not share them.
#' @param async If `TRUE`, the job runs in the background on ModelsCloud and
#'   this session checks for the result every `poll` seconds. Use it for slow
#'   methods (INLA, GP or BART with many samples).
#' @param seed Seed set on ModelsCloud before the call. If `NULL`, one is drawn
#'   from this session's random number generator, so `set.seed()` here makes a
#'   remote run repeatable.
#' @param poll Seconds between checks in asynchronous mode.
#' @param timeout Seconds to wait for an asynchronous result.
#' @param url ModelsCloud API address.
#' @param service Collection and model name of the voi service.
#' @return A list of options.
#' @export
remote_options <- function(token = Sys.getenv("MODELSCLOUD_TOKEN"), async = FALSE,
                           seed = NULL, poll = 2, timeout = 3600,
                           url = getOption("voiremote.url",
                                           "https://core.modelscloud.resp.core.ubc.ca"),
                           service = getOption("voiremote.service", "resplab/voipexa")) {
  list(token = token, async = async, seed = seed, poll = poll, timeout = timeout,
       url = url, service = service)
}

# Sends one voi call and returns voi's result as the complete R object.
.vr_run <- function(fn, args, opts) {
  if (is.null(opts$seed)) opts$seed <- sample.int(.Machine$integer.max, 1L)
  enc <- tryCatch(.vr_to_json(.vr_encode(.vr_drop_models(args))), error = function(e) {
    stop(fn, "_remote(): ", conditionMessage(e), call. = FALSE)
  })
  input <- list(fn = fn, args_json = as.character(enc), seed = opts$seed)
  res <- .vr_transport(input, opts)
  if (!isTRUE(res$ok)) {
    stop("voi::", fn, "() failed on ModelsCloud: ", res$error, call. = FALSE)
  }
  for (m in res$messages) message(m)
  for (w in res$warnings) warning(w, call. = FALSE)
  unserialize(jsonlite::base64_dec(res$rds))
}

# voi results made with check = TRUE (or by the "mm" EVSI method) carry the
# fitted regression models as "models" attributes. They hold R functions and
# can be large, and only check_regression() reads them, so they are not sent.
.vr_drop_models <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(x)
  if (!is.null(attr(x, "models", exact = TRUE))) attr(x, "models") <- NULL
  keep <- c("names", "dim", "dimnames", "class", "row.names", "levels")
  for (a in setdiff(names(attributes(x)), keep)) {
    attr(x, a) <- .vr_drop_models(attr(x, a, exact = TRUE))
  }
  if (is.list(x)) {
    cl <- oldClass(x)
    y <- unclass(x)
    for (i in seq_along(y)) if (!is.null(y[[i]])) y[[i]] <- .vr_drop_models(y[[i]])
    class(y) <- cl
    x <- y
  }
  x
}

# The arguments of the calling wrapper as the user supplied them, evaluated
# in the caller's frame. Arguments left out keep voi's own defaults.
.vr_args <- function(call, env) {
  a <- as.list(call)[-1]
  a$.remote <- NULL
  lapply(a, eval, envir = env)
}

# Turns the service output (JSON arrays of length one, as ModelsCloud returns
# them) into a plain list.
.vr_unbox <- function(out) {
  if (is.character(out)) out <- jsonlite::fromJSON(out, simplifyVector = TRUE)
  if (is.list(out) && is.null(names(out)) && length(out) == 1L) out <- out[[1]]
  lapply(out, function(v) if (is.list(v) || length(v) != 1L) v else v[[1]])
}

.vr_transport <- function(input, opts) {
  f <- getOption("voiremote.transport", .vr_http)
  .vr_unbox(f(input, opts))
}

# HTTP transport: the ModelsCloud v2 call protocol (sync or async).
.vr_http <- function(input, opts) {
  if (!nzchar(opts$token)) {
    stop("No ModelsCloud Access Key: set MODELSCLOUD_TOKEN (for example in ",
         "~/.Renviron) or pass remote_options(token = ...).", call. = FALSE)
  }
  body <- c(list(processMode = if (isTRUE(opts$async)) "Async" else "Synch"), input)
  r <- .vr_post(paste0(opts$url, "/call/v2/", opts$service), body, opts$token)
  if (identical(r$result, "running") || identical(r$result, "created")) {
    t0 <- Sys.time()
    repeat {
      Sys.sleep(opts$poll)
      r <- .vr_post(paste0(opts$url, "/execution/getResult"),
                    list(executionId = r$executionId), opts$token)
      if (!r$result %in% c("running", "created")) break
      if (as.numeric(Sys.time() - t0, units = "secs") > opts$timeout) {
        stop("No result from ModelsCloud after ", opts$timeout, " seconds; ",
             "execution ", r$executionId, " may still be running.", call. = FALSE)
      }
    }
  }
  if (!identical(r$result, "succeeded")) {
    stop("ModelsCloud call ", r$result %||% "error", ": ", r$message %||% "",
         if (!is.null(r$executionId)) paste0(" (execution ", r$executionId, ")"),
         call. = FALSE)
  }
  r$output
}

.vr_post <- function(url, body, token) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, Authorization = paste("Bearer", token),
                          "Content-Type" = "application/json")
  curl::handle_setopt(h, post = TRUE,
                      postfields = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null",
                                                    digits = NA))
  resp <- curl::curl_fetch_memory(url, handle = h)
  txt <- rawToChar(resp$content)
  if (resp$status_code >= 400) {
    stop("ModelsCloud returned HTTP ", resp$status_code, ": ", substr(txt, 1, 300),
         call. = FALSE)
  }
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Versions and optional components on the voi service
#' @param .remote Options from [remote_options()].
#' @return A list: R and voi versions, the functions offered, and whether INLA,
#'   splancs and rjags are installed there.
#' @export
voi_remote_info <- function(.remote = remote_options()) {
  .vr_transport(list(fn = "info"), .remote)
}
