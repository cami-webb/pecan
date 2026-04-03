#!/usr/bin/env Rscript

config <- config::get(file = "modules/data.land/inst/sipnet-restart-workflow/config.yml")

outdir <- file.path(config$outdir_root, "out")
ncfiles <- list.files(outdir, full.names = TRUE)
nc <- ncdf4::nc_open(ncfiles[[1]])

outdir_root <- config[["outdir_root"]]
events_json_file <- fs::path(outdir_root, "events.json")
events <- jsonlite::read_json(events_json_file, simplifyVector = FALSE)

events_df <- dplyr::bind_rows(events[[1]][["events"]]) |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  dplyr::filter(.data$event_type != "irrigation") |>
  dplyr::arrange(.data$date)

events_df |>
  dplyr::select("event_type":"crop_code")

results <- PEcAn.utils::read.output(
  ncfiles = ncfiles,
  variables = c("NEE", "LAI", "AGB", "TotSoilCarb"),
  dataframe = TRUE
) |>
  dplyr::as_tibble()

library(ggplot2)
plt <- results |>
  tidyr::pivot_longer(
    -c("posix", "year"),
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot() +
  aes(x = posix, y = value) +
  geom_line() +
  geom_vline(
    aes(xintercept = date, color = event_type),
    data = events_df,
    linetype = "dashed"
  ) +
  facet_wrap(vars(variable), scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("~/Pictures/restarts.png", plt, width = 12, height = 9, units = "in")
