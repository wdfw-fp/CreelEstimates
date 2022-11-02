# Freshwater Creel Estimates

This repository supports estimation of freshwater fishery catch and effort using roving-roving creel designs.

The main template script *fw_creel.Rmd* provide a workflow to fetch raw observation data, generate intermediate summaries, produce expanded point estimates and associated uncertainty measures, and output tables and figures. It combines and builds on previously separate scripts associated with a rapid, established method ("PE") and a more recently developed Bayesian hierarchical state space method ("BSS"). 

These standard procedures are modified through user inputs in the uppermost `params` block of the script, defining the fishery of interest (`fishery_name`), the start and end dates of the period to be assessed, the focal species and encounter types of interest (`est_catch_groups`), and various other controls related to both the sampling design/protocol (e.g., what quantities were counted during index surveys; which days of the week are considered 'weekend' vs 'weekday') and the particular analysis (e.g., a minimum 'fishing time' duration threshold to filter interview).

This parameterized Rmarkdown workflow calls a sequence of *R_functions/*  
  - `fetch_dwg` brings into memory a set of electronically collected observations from the statewide freshwater creel database [published at data.wa.gov](https://data.wa.gov/browse?q=creel)); these consist of counts at index sites and along census survey reaches as well as angler interviews and associated catch information
  - `prep_days` associates time-strata information to the period of interest (e.g., week/month index, potential fishing hours, section-specific closures)
  - `prep_dwg_interview` filters and reorganizes interview records, calculating times and joining catch values conditioned on user inputs in the `params` block
  - `prep_dwg_effort_census` filters, reorganizes and aggregates survey counts, in particular associating the "count_sequence" index of the nearest-in-time index observations
  - `prep_dwg_effort_index` similarly filters, reorganizes and aggregates counts at specific index sites 
  - `prep_inputs_bss` translates the prepared tabular data into the vector and matrix formats required by the **stan model** code and adds values for several priors
  - `fit_bss` wraps the `rstan::stan()` function, passing in the data list and various arguments controlling the MCMC process (e.g., number of chains, iterations, etc.)
  
  
  - wrangles intermediary objects and calculates estimates by expanding angler interview data according to index counts bias-corrected by census counts
  - presents various figures and tables and allows export of an Excel workbook compiling the data, intermediates and estimates

# CreelPointEstimate

Estimation of freshwater fishery catch and effort using roving-roving creel designs and following established methods (e.g., Hahn, Pollock).

[*fw_creel_pe.Rmd*](https://github.com/wdfw-fp/CreelPointEstimate/blob/main/fw_creel_pe.Rmd) is a parameterized Rmarkdown script that
 - fetches electronically collected observations from public views of the statewide freshwater creel database [published at data.wa.gov](https://data.wa.gov/browse?q=creel))
 - wrangles intermediary objects and calculates estimates by expanding angler interview data according to index counts bias-corrected by census counts
 - presents various figures and tables and allows export of an Excel workbook compiling the data, intermediates and estimates
 
 