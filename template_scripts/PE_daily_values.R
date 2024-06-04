## estimated daily values from PE period estimates 



library("tidyverse")
library("here")
library("readxl")
library("lubridate")

# run these after you have outputs from fw_cree.Rmd

# estimates_pe$effort_daily <- est_pe_effort_daily(
#   days = dwg$days,
#   pe_effort = estimates_pe$effort,
#   closures = readr::read_csv(paste(here(),"input_files/closures.csv", sep = "/"), show_col_types = FALSE) |> 
#     dplyr::filter(fishery_name == params$fishery_name))


# estimates_pe$catch_daily <- est_pe_catch_daily(
#   days = dwg$days,
#   pe_catch = estimates_pe$catch,
#   closures = readr::read_csv(paste(here(),"input_files/closures.csv", sep = "/"), show_col_types = FALSE) |> 
#     dplyr::filter(fishery_name == params$fishery_name))


estimates_pe$catch_daily <- est_pe_catch_daily(
  days = dwg$days,
  pe_catch = estimates_pe$catch,
  closures = dwg$closures |> 
    dplyr::filter(fishery_name == params$fishery_name))


# write_csv(estimates_pe$effort_daily, file = paste0(here::here("fishery_analyses", "District 14", "Estimates_06012021-11302022", "/"), 
#                                             "pe_output_effort_daily_", params$fishery_name, ".csv"))

write_csv(estimates_pe$catch_daily, file = paste0(here::here("fishery_analyses", "District 13", "Snohomish_Fisheries", "catch", "/"), 
                                                   "pe_output_catch_daily_", params$fishery_name, ".csv"))


write_csv(estimates_pe$catch_daily, "daily_catch_output.csv")



total_pre <- estimates_pe$catch |> 
  group_by(est_cg) |> 
  summarise(total_catch = sum(est))

total_post <- estimates_pe$catch_daily |> 
  group_by(est_cg) |> 
  summarise(total_catch = sum(est_catch_daily))


estimates_pe$catch_daily |> 
  ggplot(aes(x = event_date, y = est_catch_daily, color = day_type)) +
  geom_point() + 
  geom_smooth() +
  facet_wrap(.~est_cg)


estimates_pe$catch_daily |> 
  ggplot(aes(x = event_date, y = est_catch_daily)) +
  geom_col() +
  facet_wrap(.~est_cg + section_num + angler_final)

estimates_pe$catch_daily |> 
  group_by(day_type, angler_final, est_cg) |> 
  summarise(
    mean_catch = mean(est_catch_daily)
  )

estimates_pe$catch |> 
  group_by(day_type, angler_final, est_cg) |> 
  summarise(
    mean_catch = mean(est)
  ) |> 
  arrange(est_cg)

estimates_pe$catch |> 
ggplot(aes(x = period, y = est)) +
  geom_point() + 
  geom_smooth() +
  facet_wrap(.~est_cg)

# run these after you have a collection of daily files 

df_files_list <- purrr::map(list.files(here("fishery_analyses", "District 14", "Estimates_06012021-11302022", "catch"), full.names = T), readr::read_csv)

outlist <- list()

outlist$mean_daily_catch <- bind_rows(df_files_list) |> 
  dplyr::select(-c(catch_est_var, df, var, l95, u95)) |> 
  dplyr::select(fishery_name, water_body, section_num, event_date, day_type, period, week, month, year, fishery_day_index,
                fishery_week_index, fishery_month_index, catch_group = est_cg, angler_type = angler_final, mean_catch_period = est_catch_period,
                mean_catch_daily = est_catch_daily) |>
  mutate(across(where(is.numeric), round, 2))

outlist$total_catch_summary <- outlist$mean_daily_catch |>
  group_by(catch_group) |> 
  summarise(total_catch = sum(mean_catch_daily))

outlist$total_catch_summary_year <- outlist$mean_daily_catch |>
  group_by(year, water_body, catch_group) |> 
  arrange(catch_group, year) |> 
  summarise(total_catch = sum(mean_catch_daily))


writexl::write_xlsx(
  c(outlist[rev(names(outlist))]),
  path = paste0(here::here("fishery_analyses", "District 14", "/"), 
                "pe_output_catch", ".xlsx")
)



# UM Chinook encounters 

df_bind |> 
  filter(catch_group == "Chinook_Adult_UM_Released") |> 
  ggplot(aes(event_date, mean_catch_daily, fill = interaction(day_type, angler_type))) +
  geom_col(position = position_stack(), color = "black") +
  scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
  facet_wrap(~location, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)


# Native char (bull trout) encounters 

df_bind |> 
  filter(catch_group == "Bull Trout|Dolly Varden_Adult|UNK|NA_UM|NA|UNK_Released") |> 
  ggplot(aes(event_date, mean_catch_daily, fill = interaction(day_type, angler_type))) +
  geom_col(position = position_stack(), color = "black") +
  scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
  facet_wrap(~location, scales = "fixed", labeller = label_wrap_gen(multi_line = F), ncol = 2)


