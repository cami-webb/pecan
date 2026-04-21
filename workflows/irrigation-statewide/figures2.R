#!/usr/bin/env Rscript

library(ggplot2)
library(patchwork)

Sys.setenv(TAR_PROJECT = "medium")
# targets::tar_load(phenology)
targets::tar_load(parcel_waterbalance)
targets::tar_load(complete_crop_timeseries)
targets::tar_load(etref)

head(unique(parcel_waterbalance$parcel_id), 20)

pid <- 114897
dmin <- "2018-06-01"
dmax <- "2025-12-30"
wbsub <- parcel_waterbalance |>
  dplyr::filter(parcel_id == pid)
wbsub_long <- wbsub |>
  dplyr::select(
    "date", "precip_mm_day", "etref_mm_day",
    "etc_mm_day", "canopy_cover", "irr", "pond_depth",
    "runoff", "W_t", "crop_name"
  ) |>
  tidyr::pivot_longer(
    -c("date", "crop_name"),
    names_to = "variable",
    values_to = "value"
  )
vcols <- c(
  "Precipitation (mm/day)" = "precip_mm_day",
  "ET (mm/day)" = "etc_mm_day",
  "Water balance (mm)" = "W_t",
  "Irrigation (mm)" = "irr",
  "Runoff (mm)" = "runoff"
)
irrplot <- wbsub_long |>
  dplyr::filter(
    variable %in% vcols,
    date <= dmax,
    date >= dmin
  ) |>
  dplyr::mutate(variable = factor(variable, vcols, names(vcols))) |>
  ggplot() +
  aes(x = date, color = crop_name, y = value, group = 1) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(vars(variable), scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(sprintf("~/irrigation-%d.png", pid), irrplot, width = 10, height = 8, units = "in")
etref_plot <- wbsub_long |>
  dplyr::filter(
    variable %in% c("etref_mm_day", "etc_mm_day"),
    date >= dmin,
    date <= dmax
  ) |>
  dplyr::mutate(
    variable = factor(variable, c("etref_mm_day", "etc_mm_day"), c(
      "ET[ref] ~ (mm/day)",
      "ET ~ (mm/day)"
    ))
  ) |>
  ggplot() +
  aes(x = date, y = value, color = crop_name, group = 1) +
  geom_line() +
  facet_grid(
    rows = vars(variable),
    scales = "free",
    labeller = label_parsed
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(sprintf("~/etref-etc-%d.png", pid), etref_plot, width = 8, height = 6, units = "in")
