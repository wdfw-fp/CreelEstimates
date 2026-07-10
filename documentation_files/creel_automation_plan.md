# Creel Estimates: Scheduled Automation Plan (revised)

Context doc for continuing this work in Claude Code, inside the `CreelEstimates` repo.
This is a revision of the original plan after a direct review of the current code in
`CreelEstimates` and `creelutils` (v0.2.0). Corrections from that review are marked
**[revised]** with the reason.

## Goal

Run the creel estimation pipeline on a schedule (nightly) for in-season monitoring,
with results reviewed the next work day. Architecture should:

1. Work now against the public data.wa.gov (Socrata) route, on GitHub-hosted runners.
2. Switch to the internal WDFW database with a config change only — no rewrite —
   once a self-hosted runner with network/DB access is available.
3. Support pushing outputs to the agency database later, same pattern.

## [revised] What already exists — the plan's biggest blind spot

The original plan assumed a greenfield. It isn't. Two automation attempts are
already in the repo and both shape what should be built:

### 1. `render.R` + `define_schedule.R`: a working Windows Task Scheduler pipeline

The repo root already contains a production(-ish) internal-path automation:

- `define_schedule.R` uses `taskscheduleR` to register nightly Windows Task
  Scheduler jobs on a staff workstation (which has internal DB access).
- `render.R` is the script those jobs call. It loops over an `Active` list of
  fisheries and renders the **per-fishery Rmd copies** under `fishery_analyses/`
  (e.g. `fw_creel_Snohomish fall salmon 2025.Rmd`), then copies outputs to a
  Teams shared drive for next-day review.

So "goal 2" (internal path) is not future work — it already runs, just on a
different scheduler. **[decision 2026-07]** The legacy `render.R` stays untouched
until its owner is coordinated with — the Task Scheduler job calls it by path and
renders local Rmd copies in the (gitignored) `fishery_analyses/` folder, so a new
driver under a different name (`render_fisheries.R`) coexists with it with zero
interaction. Eventual replacement is still the goal; until then the new driver
must preserve the two things the current script provides that the original plan
under-specified:

- **Multi-fishery batch**: one scheduled invocation, N fisheries, failures in one
  don't stop the rest.
- **Output delivery for review**: staff currently review via the Teams drive. On
  GitHub Actions the equivalent is uploaded artifacts (or GH Pages later); the
  self-hosted-runner future can restore the Teams copy step. Plan for a
  per-target "delivery" step, not just "outputs land in `fishery_analyses/`".

Known defects in the existing `render.R` worth fixing in the new driver rather
than inheriting:

- `tryCatch(..., warning = function(w) ...)` **aborts the render on the first
  warning** (tryCatch unwinds on a caught condition; this pipeline emits routine
  warnings). Use `withCallingHandlers` for warnings, `tryCatch` only for errors.
- Always exits status 0, so the scheduler can never see a failure. The new driver
  must exit non-zero when any fishery fails.
- Hardcoded user paths (`C:/temp`, `C:/Repos`, a specific user's Teams mount) and
  a runtime `devtools::install_github("wdfw-fp/creelutils@patch_etl")` — pin
  dependencies at environment level, not inside the driver.
- The `Active` fishery list is hardcoded in the script; that's what the
  `configs/*.yml` files replace.

### 2. `.github/workflows/run_creel_analysis.yml`: exists but cannot work

There is already a `workflow_dispatch` workflow that renders an analysis folder on
`rocker/verse:4.4`. It is broken on a fresh checkout because it depends entirely on
**gitignored paths**:

- `.devcontainer/` (Dockerfile, `install_packages.R`) — gitignored, so the
  "Install creelutils" step and the cache key referencing those files always fail.
- Its input is a path under `fishery_analyses/` containing a `run_rmd.R` — also
  gitignored, so the folder never exists in CI.

Keep its good ideas (rocker container, Posit Package Manager binaries for fast
installs, 360-min timeout, artifact upload) and replace it with a workflow that
depends only on tracked files. Decide explicitly whether to delete or repurpose
this file — don't leave two workflows.

## Key existing pieces already in the repo

- `template_scripts/fw_creel.Rmd` — parameterized template, single source of truth.
  Do **not** fork into a separate in-season `.Rmd`. **[revised]** Note the current
  Task Scheduler flow *does* render forked per-fishery copies (created by
  `save_analysis_metadata()`); the config-driven driver renders the template
  directly and makes those copies archival snapshots only.
- **[revised]** `param_hash <- digest::digest(list(params, est_dates))` (not just
  `params`) already exists — extend this pattern into a full run manifest
  (timestamp, git SHA, config, data source, success/failure).
- `analysis_lut` / `outputs_folders` structure — self-organizing per analysis ID.
  Keep this; automation should slot into it, not replace it.
- `params$export` already has `"local"` vs `"database"` branches (bottom of
  `fw_creel.Rmd`). The `"database"` branch calls `connect_creel_db()`, so configs
  on the external/GitHub-hosted path must use `export: "local"`.

## `creelutils::fetch_data()` — the real data-source switch

```r
fetch_data(
  conn = NULL,
  fishery_name,
  tables = c("effort", "ll", "interview", "catch", "closures",
             "fishery_manager", "creel_event", "model_catch_group"),
  data_source = c("internal", "external")
)
```

- `data_source = "internal"` → WDFW Postgres via `conn` (auto-opens/closes
  `connect_creel_db()` if `conn = NULL`).
- `data_source = "external"` → data.wa.gov Socrata CSV endpoints, no DB needed.
- Returns same-shaped list of tibbles either way; `.standardize_types()` already
  coerces internal Postgres types to match Socrata/readr types. Cross-source
  consistency is handled — don't re-solve this.
- Fails loudly on bad connection, bad table name, or Socrata 429. No silent
  fallback between sources.
- **Gotcha**: `data_source` has no real default — `match.arg()` silently picks
  `"internal"` if omitted, **including when `NULL` is passed**. Never rely on the
  default in automation code, and don't declare the Rmd param as `null`.
- **[revised]** `model_catch_group` is a non-issue for this pipeline: `fw_creel.Rmd`
  never uses it (`fetch_dwg()` doesn't request it). The automation call should
  simply request the same six tables `fetch_dwg()` uses, sidestepping the
  NULL-on-external behavior entirely.
- **[revised]** Switching the Rmd from `fetch_dwg()` to a direct `fetch_data()`
  call **loses a guard**: `fetch_dwg()` aborts when *all* tables come back with 0
  rows (which is what a typo'd `fishery_name` produces on the external path —
  empty CSVs, no error). If calling `fetch_data()` directly, re-add that
  zero-row check in the `dwg_fetch` chunk; otherwise the render fails much later
  with a confusing error.

## [revised] `resolve_dates()` — the hidden internal-DB dependency

This is the most important correction. `fw_creel.Rmd` calls `resolve_dates()`
*before* fetching data. When either date param is blank, it queries
`creelutils::fishery_lut()`, which is **internal-Postgres-only** (it goes through
`connect_creel_db()`; fishery start/end dates are not on the Socrata mirrors —
`.standardize_types()` even drops `fishery_start_date`/`fishery_end_date` from the
external `fishery_manager` table). Consequences:

- Blank dates on a GitHub-hosted runner → `cli_abort`, render dead on arrival.
- Fully explicit dates in a static config → works, but **freezes the window**,
  defeating nightly in-season monitoring (the whole point is
  `est_date_end = yesterday`, which `resolve_dates()` only provides via the DB).

**Fix (external-path-safe, no creelutils change needed):** the driver computes the
rolling window. Config supplies an explicit `est_date_start` and leaves
`est_date_end: ""`; `render.R` replaces the blank end date with
`as.character(Sys.Date() - 1)` before calling `rmarkdown::render()`. With both
dates supplied, `resolve_dates()` passes them through without touching the DB.
On the internal path the driver can simply leave both blank and let
`resolve_dates()` do its DB lookup as designed.

(Longer term, publishing fishery dates to data.wa.gov or adding an external route
to `fishery_lut()` would let the Rmd own this again — note it as a creelutils
issue, don't block on it.)

## Changes needed in `fw_creel.Rmd`

1. Add to params block:
   ```yaml
   data_source: "external"   # "internal" or "external" — validated in setup
   run_mode: "interactive"   # "interactive" or "automated"
   ```
   **[revised]** The original plan said "no default". Rmd YAML params must carry a
   value, and `null` is dangerous here (`match.arg(NULL)` silently resolves to
   `"internal"`). Default to `"external"` (safe anywhere, no credentials) and add
   an explicit validation line in the `setup` chunk:
   ```r
   stopifnot(params$data_source %in% c("internal", "external"))
   ```
   The driver separately refuses configs that don't state `data_source` — the
   "force it explicit" discipline lives in automation, where it matters.

2. Replace the `dwg_fetch` chunk's fetch call:
   ```r
   dwg <- creelutils::fetch_data(
     fishery_name = params$fishery_name,
     tables       = c("effort", "ll", "interview", "catch", "closures", "fishery_manager"),
     data_source  = params$data_source
   )
   if (all(purrr::map_int(dwg, nrow) == 0)) {
     cli::cli_abort("No data returned for {.val {params$fishery_name}} from {params$data_source} source.")
   }
   ```
   (rest of that chunk — `prep_days()` etc. — stays the same)

3. **[revised]** `run_mode` currently gates nothing. Wire it to at least:
   suppress interactive-only output, and skip/guard anything that assumes RStudio
   (`rstudioapi` fallbacks already exist). Fine to add the param now and grow into
   it, but don't claim behavior it doesn't have.

4. **[revised]** Latent bug worth fixing while in the file: the "Fishery dates"
   chunk uses `eval=params$report_type %in% c("detailed","summary")` but
   `report_type` is **not a declared param** — `params$report_type` is `NULL`, the
   condition evaluates to `logical(0)`, and the chunk is silently skipped. Either
   declare `report_type` or drop the condition. As-is, anyone adding
   `report_type` to a config gets an rmarkdown error (undeclared params are
   rejected by `rmarkdown::render()`).

## Where do analyst-owned, fishery-specific pieces live?

The team's instinct (and current practice) is a stable of fishery-specific Rmds:
copies of the template where the analyst sets catch groups and params in the
header and inserts manual data edits inline, with the workflow iterating the
stable. The underlying requirement is right — analysts need a per-fishery place
they own — but full template forks are the expensive way to get it:

- The estimation code (~1,500 lines) is duplicated per fishery per season. A
  template fix mid-season doesn't propagate; two fisheries can silently run
  different code and nothing surfaces it.
- The forks would have to move out of gitignored `fishery_analyses/` into
  tracked space for CI to see them.
- The rolling in-season window gets awkward: a fork's header is supposed to be
  the source of truth, but the driver still has to override `est_date_end` at
  render time, giving two competing sources of params.

**Recommended shape — same analyst ownership, ~95% less duplication.** Each
fishery is defined by two small analyst-owned files, and the template stays the
single source of code:

1. `configs/<fishery>.yml` — all params, including `est_catch_groups`. Same YAML
   the analyst would have written in an Rmd header, minus the `!r
   data.frame(rbind(...))` awkwardness.
2. `configs/edits/<fishery>.R` — **optional** manual data edits, applied by the
   template's existing (currently empty) `manual_edits` chunk:

   ```r
   edits_file <- here("configs", "edits",
                      paste0(gsub("[^A-Za-z0-9]", "_", params$fishery_name), ".R"))
   if (file.exists(edits_file)) {
     cli::cli_alert_info("Applying manual data edits from {.file {edits_file}}")
     cat(readLines(edits_file), sep = "\n")   # echo edits into the report
     source(edits_file, local = TRUE)          # runs in knit env; can modify dwg
   } else {
     cli::cli_alert_info("No manual edits file for this fishery.")
   }
   ```

   Echoing the file into the rendered report preserves the best property of
   inline edits — reviewers see exactly what was changed, with the analyst's
   comments, in context. Record the edits file's hash in the run manifest for
   provenance.

The workflow loop is identical either way (iterate configs vs. iterate Rmds), so
this choice doesn't block the CI work — and per-run Rmd snapshots via
`save_analysis_metadata()` still land in each analysis folder, preserving the
"what exactly ran" record the forks were providing.

## `render_fisheries.R` driver (new file; legacy `render.R` untouched)

Responsibilities:
- Accept one or more config files (one YAML per fishery under `configs/`);
  loop with per-fishery `tryCatch` so one failure doesn't stop the batch —
  this replaces the `Active` list in the current script.
- Validate `data_source` is explicitly `"internal"` or `"external"` before doing
  anything else; validate config params against the Rmd's declared params
  (**[revised]** `rmarkdown::render()` errors on undeclared params, so the config
  schema must match the Rmd header exactly — keep driver-only keys like
  `output_dir` in a separate top-level block, not inside `params`).
- **[revised]** Resolve the rolling date window (see `resolve_dates()` section):
  external configs get `est_date_end <- as.character(Sys.Date() - 1)` when blank.
- **[revised]** Build `est_catch_groups` from the config. The template's default
  is Skagit Chinook groups; a config that omits catch groups silently estimates
  the wrong fish. YAML can't express a data.frame directly — configs list them as
  maps and the driver does `dplyr::bind_rows()`:
  ```yaml
  est_catch_groups:
    - {species: "Steelhead", life_stage: "Adult", fin_mark: "UM", fate: "Released"}
    - {species: "Steelhead", life_stage: "Adult", fin_mark: "AD", fate: "Kept"}
  ```
  Make `est_catch_groups` **required** in every config.
- Call `rmarkdown::render()` on `template_scripts/fw_creel.Rmd` with those params,
  `envir = new.env()`, and an explicit `output_dir`/`output_file` (by default
  `render()` drops the HTML next to the template).
- Wrap errors in `tryCatch` but let warnings pass through
  (`withCallingHandlers` to log them) — the current script's warning handler
  kills renders.
- Per-fishery manifest: timestamp, git SHA (`Sys.getenv("GITHUB_SHA", unset = NA)`
  falling back to `git rev-parse HEAD`), param hash, data source, runtime,
  status, error message. Exit non-zero if any fishery failed (so CI marks the
  job failed) after finishing the rest of the batch.
- **[revised — dropped]** The original plan said: when running multiple internal
  fisheries, open one `connect_creel_db()` and pass it as `conn` through render
  params. Don't. A DBI connection inside `params` flows into
  `digest::digest(list(params, est_dates))` (external pointers hash unstably,
  breaking `cache.extra`) and into the params YAML snapshot written by
  `save_analysis_metadata()`. Let each render open its own lazy connection via
  `fetch_data(conn = NULL)`; connection setup cost is noise next to a Stan run.

Skeleton:

```r
#!/usr/bin/env Rscript
# render_fisheries.R — automation driver for fw_creel.Rmd
# (distinct from legacy render.R, which the Task Scheduler job still owns)
# Usage: Rscript render_fisheries.R --config configs/hoh_river_external.yml [more configs...]

library(yaml)
library(rmarkdown)
library(digest)
library(cli)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

config_paths <- commandArgs(trailingOnly = TRUE) |> setdiff("--config")
stopifnot(length(config_paths) > 0)

run_one <- function(config_path) {
  config <- yaml::read_yaml(config_path)

  if (!isTRUE(config$data_source %in% c("internal", "external"))) {
    stop("data_source must be explicitly 'internal' or 'external'. Got: ",
         config$data_source %||% "NULL")
  }
  if (is.null(config$est_catch_groups)) {
    stop("est_catch_groups is required in every config (the template default is fishery-specific).")
  }
  config$est_catch_groups <- dplyr::bind_rows(config$est_catch_groups)

  # Rolling in-season window without touching the internal DB (see plan doc):
  if (config$data_source == "external" && !nzchar(config$est_date_end %||% "")) {
    config$est_date_end <- as.character(Sys.Date() - 1)
  }

  run_start <- Sys.time()
  git_sha <- Sys.getenv("GITHUB_SHA", unset = NA)
  if (is.na(git_sha)) {
    git_sha <- tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) NA)
  }

  result <- tryCatch({
    withCallingHandlers(
      rmarkdown::render(
        input       = "template_scripts/fw_creel.Rmd",
        params      = config,
        output_file = paste0("fw_creel_", gsub("[^A-Za-z0-9]", "_", config$fishery_name), ".html"),
        envir       = new.env()
      ),
      warning = function(w) {
        cli::cli_alert_warning("[{config$fishery_name}] {conditionMessage(w)}")
        invokeRestart("muffleWarning")   # log, don't die
      }
    )
    list(status = "success", error = NA)
  }, error = function(e) list(status = "failure", error = conditionMessage(e)))

  manifest <- list(
    timestamp   = as.character(run_start),
    git_sha     = git_sha,
    config_file = config_path,
    data_source = config$data_source,
    fishery     = config$fishery_name,
    est_date_end = config$est_date_end,
    param_hash  = digest::digest(config),
    status      = result$status,
    error       = result$error,
    runtime_sec = as.numeric(difftime(Sys.time(), run_start, units = "secs"))
  )

  dir.create("run_manifests", showWarnings = FALSE)
  jsonlite::write_json(
    manifest,
    file.path("run_manifests",
              paste0("manifest_", gsub("[^A-Za-z0-9]", "_", config$fishery_name), "_",
                     format(run_start, "%Y%m%d_%H%M%S"), ".json")),
    auto_unbox = TRUE
  )
  manifest
}

manifests <- lapply(config_paths, function(p) {
  tryCatch(run_one(p),
           error = function(e) list(status = "failure", config_file = p,
                                    error = conditionMessage(e)))
})

failed <- vapply(manifests, function(m) identical(m$status, "failure"), logical(1))
for (m in manifests[failed]) cli::cli_alert_danger("{m$config_file %||% m$fishery}: {m$error}")
if (any(failed)) quit(status = 1)
cli::cli_alert_success("{length(manifests)} run{?s} completed")
```

## GitHub Actions workflow (public-data route, works now)

**[revised]** Two corrections to the original draft:

- **No renv.** The repo has no `renv.lock`, so `r-lib/actions/setup-renv` fails
  immediately. Either adopt renv first (a separate chore) or — simpler, and
  consistent with the existing broken workflow's approach — run in
  `rocker/verse:4.4` and install the delta from Posit Package Manager binaries.
  The package list must live in a **tracked** file (e.g. `ci/install_packages.R`),
  not the gitignored `.devcontainer/`.
- **No CmdStan cache.** The pipeline uses **rstan** (`fit_bss()` → `rstan::stan`),
  not cmdstanr; `~/.cmdstan` never exists. What's actually worth caching is
  rstan's compiled model: `rstan_options(auto_write = TRUE)` writes a compiled
  `.rds` next to the `.stan` file, so cache `stan_models/*.rds` keyed on OS +
  R version + hash of the `.stan` files. Note the `.rds` files already committed
  in `stan_models/` were compiled on Windows — rstan will recompile on Linux
  (several minutes, once per cache miss); consider whether committed compiled
  artifacts should be gitignored instead.

```yaml
# .github/workflows/nightly-creel.yml
on:
  schedule:
    - cron: '0 9 * * *'   # UTC — GH cron can drift under load, budget a buffer
  workflow_dispatch: {}    # manual trigger for testing

jobs:
  run-creel:
    runs-on: ubuntu-latest
    container:
      image: rocker/verse:4.4    # R + tidyverse + rmarkdown + pandoc preinstalled
    timeout-minutes: 350         # hosted runners cap at 360
    steps:
      - uses: actions/checkout@v4
      - name: Cache R library
        uses: actions/cache@v4
        with:
          path: /usr/local/lib/R/site-library
          key: r-lib-${{ hashFiles('ci/install_packages.R') }}
      - name: Cache compiled Stan models
        uses: actions/cache@v4
        with:
          path: stan_models/*.rds
          key: stan-${{ runner.os }}-${{ hashFiles('stan_models/*.stan') }}
      - name: Install R packages (PPM binaries) + creelutils
        run: Rscript ci/install_packages.R
      - name: Run creel model
        env:
          SOCRATA_APP_TOKEN: ${{ secrets.SOCRATA_APP_TOKEN }}
        run: Rscript render_fisheries.R --config configs/hoh_river_external.yml
      - uses: actions/upload-artifact@v4
        if: always()
        with:
          name: creel-run-${{ github.run_id }}
          path: |
            fishery_analyses/
            run_manifests/
      - name: Notify on failure
        if: failure()
        run: echo "Add Slack/email/Teams notification step here"
```

Example config (`configs/hoh_river_external.yml`):
```yaml
project_name: "District 16"
fishery_name: "Hoh winter steelhead 2022-23"   # must match the fishery_name in the data exactly
est_date_start: "2022-12-01"
est_date_end: ""            # blank = driver fills in yesterday (rolling in-season window)
est_catch_groups:
  - {species: "Steelhead", life_stage: "Adult", fin_mark: "UM", fate: "Released"}
  - {species: "Steelhead", life_stage: "Adult", fin_mark: "AD", fate: "Kept"}
data_source: "external"
run_mode: "automated"
enable_cache: false         # knitr cache is useless on ephemeral runners and risks staleness
export: "local"             # "database" requires connect_creel_db(); internal path only
```

**[revised]** `enable_cache: true` (original plan) buys nothing on a fresh runner —
the `*_cache/` dirs don't persist — and stale-cache bugs are the classic knitr
failure mode. Keep it `false` for CI.

**[revised]** The config must carry every remaining declared param the fishery
needs (`study_design`, `period_pe`, `bss_model_file_name`, `model_used`, etc.)
or accept the template defaults knowingly — `rmarkdown::render()` merges config
params over header defaults, and the header defaults are Skagit-specific in
places. Copy the full params block into the first config and prune deliberately.

## When the internal DB path is ready

- Same `render.R`, same workflow structure — just:
  - `data_source: "internal"` in the config, and blank dates are then allowed
    (the Rmd's `resolve_dates()` does the DB lookup itself).
  - Runner changes from `ubuntu-latest` to a **self-hosted runner** inside the
    WDFW network. **[revised]** Interim option that exists *today*: the Windows
    Task Scheduler box already runs the internal path nightly; the migration is
    moving that box from the legacy `render.R` to the new config-driven one, then
    to a proper runner.
  - Credentials for `connect_creel_db()` live on that runner (it reads the
    gitignored `config.yml`; keep that mechanism) rather than `SOCRATA_APP_TOKEN`.
  - `export: "database"` becomes available, restoring automated uploads.
- No changes needed to `fw_creel.Rmd` or `fetch_data()` calls.

## Pitfalls to test for before trusting the nightly schedule

- **Stan compile + sampler time** — `fit_bss()` compiles via rstan on first run
  per runner; cache `stan_models/*.rds` as above. Current dev settings
  (`n_iter = 50, n_warmup = 25`, hardcoded in the `estimates_bss` chunk — note
  these are **not params**, so production settings require an Rmd edit or new
  params) are placeholders; time a real production run before trusting the
  timeout. **[revised]** Sampler settings being non-parameterized is itself a
  gap for automation — promote `n_iter`/`n_warmup`/`n_chain` to params while
  editing the Rmd.
- **Memory** — **[revised]** public-repo `ubuntu-latest` runners have 16 GB
  (4 vCPU) since early 2024; the 7 GB figure applies to private repos. Still
  watch it: the existing `gc()` calls around draws handling show this pipeline
  has hit memory pressure before.
- **`save_draws <- FALSE`** is also hardcoded mid-chunk — fine default for
  automation, but same parameterization argument applies.
- **`data_source` footgun** — always pass it explicitly; `match.arg()` silently
  picks `"internal"` on `NULL`/missing.
- **Empty-data behavior on external** — a wrong `fishery_name` returns empty
  tibbles, not an error; the zero-row guard in the `dwg_fetch` chunk is the
  backstop. Test with a deliberately bad name.
- **Rolling window edge cases** — first day of a fishery (`Sys.Date() - 1 <
  est_date_start` → `resolve_dates()` aborts on inverted window; decide whether
  the driver should skip-with-manifest instead of fail), and days with no new
  survey data.
- **Socrata data latency** — nightly runs are only useful if the public mirrors
  update nightly; confirm the data.wa.gov refresh cadence before promising
  "yesterday's data" to reviewers. On the internal path this concern disappears.

## Immediate next steps (doable now, public-data route only)

1. Add `data_source` + `run_mode` params (with validation) to `fw_creel.Rmd`;
   swap `dwg_fetch` to `fetch_data()` with the explicit six-table list and the
   zero-row guard; fix the undeclared `report_type` chunk option; promote BSS
   sampler settings to params.
2. Add `render_fisheries.R` per the skeleton. Legacy `render.R` stays untouched
   until the Snohomish nightly job's owner is coordinated with; retiring it is a
   later, separate step.
3. Add `ci/install_packages.R` (tracked; replaces the gitignored
   `.devcontainer/install_packages.R` the old workflow depended on).
4. Add `configs/` with one real fishery config including `est_catch_groups`.
5. Replace `run_creel_analysis.yml` with `nightly-creel.yml`; test via
   `workflow_dispatch` before trusting the schedule; add `SOCRATA_APP_TOKEN`
   as a repo secret.
6. Time a full run at real (non-dev) sampler settings before committing to a
   nightly cadence.
7. File a creelutils issue: external route for fishery dates
   (`fishery_lut()`/Socrata) so `resolve_dates()` can eventually work
   DB-free and the driver-side date hack can be removed.
