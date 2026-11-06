# One wrapper per voi function. Each has voi's own arguments plus `.remote`,
# sends exactly the arguments given, and returns what voi returns.

#' Expected value of perfect information, on ModelsCloud
#'
#' Same as [voi::evpi()], run remotely.
#' @inheritParams voi::evpi
#' @param .remote Options from [remote_options()].
#' @return As [voi::evpi()].
#' @export
evpi_remote <- function(outputs, nsim = NULL, .remote = remote_options()) {
  .vr_run("evpi", .vr_args(match.call(), parent.frame()), .remote)
}

#' Expected value of partial perfect information, on ModelsCloud
#'
#' Same as [voi::evppi()], run remotely, including `method = "inla"`, which
#' needs no local INLA installation.
#' @inheritParams voi::evppi
#' @param .remote Options from [remote_options()].
#' @return As [voi::evppi()]: the result can be plotted with `plot()` when
#'   voi is loaded.
#' @export
evppi_remote <- function(outputs, inputs, pars = NULL, method = NULL, se = FALSE,
                         B = 1000, nsim = NULL, verbose = FALSE, check = FALSE, ...,
                         .remote = remote_options()) {
  .vr_run("evppi", .vr_trim_inputs(.vr_args(match.call(), parent.frame())), .remote)
}

#' EVPPI for the variance of an outcome, on ModelsCloud
#'
#' Same as [voi::evppivar()], run remotely.
#' @inheritParams voi::evppivar
#' @param .remote Options from [remote_options()].
#' @return As [voi::evppivar()].
#' @export
evppivar_remote <- function(outputs, inputs, pars = NULL, method = NULL, nsim = NULL,
                            verbose = TRUE, ..., .remote = remote_options()) {
  .vr_run("evppivar", .vr_trim_inputs(.vr_args(match.call(), parent.frame())), .remote)
}

#' Expected value of sample information, on ModelsCloud
#'
#' Same as [voi::evsi()], run remotely. For now only voi's built-in study
#' designs (`study = "binary"`, `"trial_binary"` or `"normal_known"`) can be
#' used: arguments that are R functions (`datagen_fn`, `likelihood`,
#' `analysis_fn`, `model_fn`, `par_fn`) cannot be sent.
#' @inheritParams voi::evsi
#' @param .remote Options from [remote_options()].
#' @return As [voi::evsi()].
#' @export
evsi_remote <- function(outputs, inputs, study = NULL, datagen_fn = NULL, pars = NULL,
                        pars_datagen = NULL, n = 100, aux_pars = NULL, method = NULL,
                        likelihood = NULL, analysis_fn = NULL, analysis_args = NULL,
                        model_fn = NULL, par_fn = NULL, Q = 50, npreg_method = "gam",
                        nsim = NULL, verbose = FALSE, check = FALSE, ...,
                        .remote = remote_options()) {
  .vr_run("evsi", .vr_args(match.call(), parent.frame()), .remote)
}

#' EVSI for the variance of an outcome, on ModelsCloud
#'
#' Same as [voi::evsivar()], run remotely, with voi's built-in study designs
#' only (see [evsi_remote()]).
#' @inheritParams voi::evsivar
#' @param .remote Options from [remote_options()].
#' @return As [voi::evsivar()].
#' @export
evsivar_remote <- function(outputs, inputs, study = NULL, datagen_fn = NULL, pars = NULL,
                           n = 100, aux_pars = NULL, method = NULL, nsim = NULL,
                           verbose = TRUE, ..., .remote = remote_options()) {
  .vr_run("evsivar", .vr_args(match.call(), parent.frame()), .remote)
}

#' Expected net benefit of sampling, on ModelsCloud
#'
#' Same as [voi::enbs()], run remotely.
#' @inheritParams voi::enbs
#' @param .remote Options from [remote_options()].
#' @return As [voi::enbs()].
#' @export
enbs_remote <- function(evsi, costs_setup, costs_pp, pop, time, dis = 0.035,
                        smooth = FALSE, smooth_df = NULL, pcut = 0.05,
                        .remote = remote_options()) {
  .vr_run("enbs", .vr_args(match.call(), parent.frame()), .remote)
}

#' Optimal sample size from expected net benefit of sampling, on ModelsCloud
#'
#' Same as [voi::enbs_opt()], run remotely.
#' @inheritParams voi::enbs_opt
#' @param .remote Options from [remote_options()].
#' @return As [voi::enbs_opt()].
#' @export
enbs_opt_remote <- function(x, pcut = 0.05, smooth = FALSE, smooth_df = NULL,
                            keep_preds = FALSE, .remote = remote_options()) {
  .vr_run("enbs_opt", .vr_args(match.call(), parent.frame()), .remote)
}

#' Population value of information, on ModelsCloud
#'
#' Same as [voi::pop_voi()], run remotely.
#' @inheritParams voi::pop_voi
#' @param .remote Options from [remote_options()].
#' @return As [voi::pop_voi()].
#' @export
pop_voi_remote <- function(voi, pop, time, dis = 0.035, .remote = remote_options()) {
  .vr_run("pop_voi", .vr_args(match.call(), parent.frame()), .remote)
}

# EVPPI uses only the columns of `inputs` named in `pars`, so only those are
# sent; the result is the same and the request much smaller.
.vr_trim_inputs <- function(args) {
  x <- args$inputs
  p <- unique(unlist(args$pars))
  if ((is.data.frame(x) || is.matrix(x)) && length(p) > 0 && all(p %in% colnames(x))) {
    args$inputs <- x[, colnames(x) %in% p, drop = FALSE]
  }
  args
}
