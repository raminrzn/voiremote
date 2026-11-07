# voiremote

Run the [voi](https://chjackson.github.io/voi/) package (value of information,
Jackson et al.) on UBC ModelsCloud. Each function takes the same arguments as
its voi counterpart and returns the same R object, computed remotely, so
methods needing INLA can be used without installing INLA.

| Remote | voi |
|---|---|
| `evpi_remote()` | `evpi()` |
| `evppi_remote()` | `evppi()`, including `method = "inla"` |
| `evppivar_remote()` | `evppivar()` |
| `evsi_remote()` | `evsi()`, built-in study designs only |
| `evsivar_remote()` | `evsivar()`, built-in study designs only |
| `enbs_remote()` | `enbs()` |
| `enbs_opt_remote()` | `enbs_opt()` |
| `pop_voi_remote()` | `pop_voi()` |

`voi` itself is installed as a dependency (it does not need INLA), so
`plot()`, `check_regression()` and the other voi functions work on the
results.

## Setup

1. Create an Access Key on ModelsCloud (user menu → Access Keys).
2. Add it to `~/.Renviron` as `MODELSCLOUD_TOKEN=<your key>` and restart R.

Keys are personal; do not share them.

```r
remotes::install_github("raminrzn/voiremote")
library(voiremote)
evppi_remote(voi::chemo_nb, voi::chemo_pars,
             pars = list("p_side_effects_t1", c("p_side_effects_t1", "p_side_effects_t2")))
```

## Options

The last argument of each function, `.remote = remote_options(...)`, sets:

- `async = TRUE`: run in the background and poll for the result. Use it for
  slow fits.
- `seed`: the seed used on ModelsCloud. By default one is drawn from your R
  session, so `set.seed()` makes remote runs repeatable.
- `token`, `url` and `service`.

## Limits

- **Only data can be sent.** Arguments that are R functions are refused:
  `evppi_mc()`, and EVSI with a custom `datagen_fn`, `analysis_fn` or similar.
  Use voi locally for those.
- **Fitted models are not sent.** Models attached to voi results (from
  `check = TRUE`) are dropped before sending; only `check_regression()` uses
  them.

## Validation

The test suite runs the service package locally and compares every function
with voi itself: EVPPI by gam, gp, earth, so, sal and bart, with bootstrap
SEs; EVPPI of a variance; EVSI by gam and importance sampling; ENBS (voi's
own test case, -9,250,000); optimal sample size; and population VoI. Results
are identical, given the same seed for random methods.

Live check on ModelsCloud (`/resplab/voipexa`, 2026-11-07): every feature
that needs neither INLA nor JAGS was run on the service with voi's example
data (10,000 samples, sent by reference with `voi_data()`), asynchronously,
and compared with voi run locally:

| Function | Variants run | Largest relative difference |
|---|---|---|
| `evpi` | net benefit; costs and effects, 5 WTP values | 4e-11 |
| `evppi` | gam (1 and 2 parameters); costs and effects | 5e-8 |
| `evppi` | gp, earth, bart, so, sal; bootstrap SE | 9e-12 |
| `evppivar` | 1 and 2 parameters | 9e-12 |
| `evsi` | `trial_binary` by gam and by importance sampling; `binary` | 6e-9 |
| `evsivar` | `binary` | 1e-11 |
| `enbs` | voi's test case (first ENBS -9,250,000; 15,030 rows) | 2e-16 |
| `pop_voi` | 3 values | 0 |

Random methods (bart, bootstrap SE, EVSI) gave the same answers given the
same seed. The small differences come from the service's newer R (4.5) and
packages; the data arrive bit for bit. `enbs_opt` was checked through the
local service only, as its input (the ENBS table) exceeds the request limit.

## Status

The service is in Test, not yet published. Two things are needed on the
server, from the platform administrators:

- **INLA** (`method = "inla"`). INLA installs, but its `fmesher` dependency
  needs `sf`, which needs the GDAL, GEOS, PROJ and udunits system libraries.
- **JAGS** (`evsi(method = "mm")`).

Requests are limited to about 1 MB for now, roughly 90,000 numbers. EVPPI
sends only the parameter columns analysed; otherwise reduce `nsim`.

## Licence

GPL-3, as voi.
