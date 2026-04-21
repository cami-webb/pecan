#!/usr/bin/env Rscript

library(ggplot2)
library(patchwork)

Sys.setenv(TAR_PROJECT = "medium")
# targets::tar_load(phenology)
targets::tar_load(parcel_waterbalance)
targets::tar_load(complete_crop_timeseries)
targets::tar_load(etref)

head(unique(parcel_waterbalance$parcel_id), 20)

# pid <- c(39011, 59465)
pid <- c(39230, 86888)
# dsub <- complete_crop_timeseries |>
#   dplyr::filter(parcel_id %in% pid)
wbsub <- parcel_waterbalance |>
  dplyr::filter(parcel_id %in% pid)
wbsub_long <- wbsub |>
  dplyr::select(
    "parcel_id", "date", "precip_mm_day", "etref_mm_day",
    "etc_mm_day", "canopy_cover", "irr", "pond_depth",
    "runoff", "W_t", "crop_name"
  ) |>
  tidyr::pivot_longer(
    -c("parcel_id", "date", "crop_name"),
    names_to = "variable",
    values_to = "value"
  )
vcols <- c(
  "Water balance (mm)" = "W_t",
  "Pond depth (mm)" = "pond_depth",
  "Irrigation (mm)" = "irr",
  "Runoff (mm)" = "runoff"
)
irrplot <- wbsub_long |>
  dplyr::filter(variable %in% vcols) |>
  dplyr::mutate(variable = factor(variable, vcols, names(vcols))) |>
  ggplot() +
  aes(x = date, color = crop_name, y = value, group = 1) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(rows = vars(variable), cols = vars(parcel_id), scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("~/irrigation.png", irrplot, width = 10, height = 8, units = "in")
etref_plot <- wbsub_long |>
  dplyr::filter(
    variable %in% c("etref_mm_day", "etc_mm_day")
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
    cols = vars(parcel_id),
    scales = "free",
    labeller = label_parsed
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("~/etref-etc.png", etref_plot, width = 8, height = 6, units = "in")

# wbsub_long |>
#   dplyr::filter(
#     variable %in% c("precip_mm_day", "etc_mm_day", "irr", "W_t")
#   ) |>
#   dplyr::mutate(
#     variable = factor(
#       variable,
#       c("")
#     )
#   )
# ggplot(wbsub_long) +
#   aes(x = date, y = value, color = crop_name, group = 1) +
#   geom_line() +
#   facet_grid(rows = vars(variable), cols = vars(parcel_id), scales = "free") +
#   theme_bw()
#
# dsub_long <- dsub |>
#   dplyr::select(
#     "parcel_id", "date", "precip_mm_day", "etref_mm_day", "etc_mm_day",
#     "whc_min_frac", "whc_mm", "canopy_cover", "crop_name"
#   ) |>
#   tidyr::pivot_longer(
#     -c("parcel_id", "date", "crop_name"),
#     names_to = "variable",
#     values_to = "value"
#   )
#
# ggplot(dsub_long) +
#   aes(x = date, y = value, color = crop_name, group = 1) +
#   geom_line() +
#   facet_grid(rows = vars(variable), cols = vars(parcel_id), scales = "free")
#
# etsub <- etref |>
#   dplyr::filter(parcel_id %in% pid)
#
# ggplot(etsub) +
#   aes(x = date, y = etref_mm_day) +
#   geom_line() +
#   facet_grid(rows = "parcel_id")
#
#
# ggplot(pheno_sub) +
#   aes(x = date, y = canopy_cover, color = parcel_id) +
#   geom_line()
