#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(readr)

sipnet_out_file <- "modules/data.land/inst/sipnet-restart-workflow/_test/segments/segment_001/out/1/sipnet.out"

skip_n <- 0
sipnet_output <- tryCatch({
  utils::read.table(sipnet_out_file, header = TRUE, skip = skip_n, sep = "")
}, error = function(err) {
  PEcAn.logger::logger.warn(
    "Failed to read using `read.table`. ",
    "Trying to parse output manually."
  )
  raw_lines <- readLines(sipnet_out_file)
  raw_header <- raw_lines[[1 + skip_n]]
  raw_body <- tail(raw_lines, -(1 + skip_n))
  # SIPNET output is right-aligned with the column names in the header.
  # We use this to figure out where the numbers end if there are no spaces.
  token_matches <- gregexpr("\\S+", raw_header, perl = TRUE)
  proc_header <- regmatches(raw_header, token_matches)[[1]]
  col_ends <- token_matches[[1]] + attr(token_matches[[1]], "match.length") - 1
  col_starts <- c(1, head(col_ends, -1) + 1)
  col_widths <- col_ends - col_starts + 1
  result <- read.fwf(
    textConnection(raw_body),
    widths = col_widths,
    col.names = proc_header,
    na.strings = c("nan", "-nan")
  )
  result[] <- lapply(result, as.numeric)
  result
})

dat <- as_tibble(sipnet_output) |>
  mutate(
    date = PEcAn.SIPNET:::sipnet2datetime(year, day, time),
    .before = 0
  )

head(dat, 20)

dwide <- dat |>
  select(-c("year", "day", "time")) |>
  tidyr::pivot_longer(
    -"date",
    names_to = "variable",
    values_to = "value"
  )

dwide |>
  # filter(date < "2016-07-15") |>
  # filter(date < "2016-06-11") |>
  ggplot() +
  aes(x = date, y = value) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free")

ggsave("~/Pictures/bad-segment-early.png")

# Find first NA
first_na_index <- which(rowSums(is.na(dat)) > 0)[1]
start_row <- pmax(1, first_na_index - 10)
end_row   <- pmin(nrow(dat), first_na_index + 10)
result <- dat |>
  slice(start_row:end_row)

print(dat, n = Inf)

dat |>
  dplyr::filter(is.na(litter))

tail(dat, 30) |> dplyr::glimpse()

ncfile <- "modules/data.land/inst/sipnet-restart-workflow/_test/segments/segment_001/out/1/2016.nc"
vnames <- c("")
dnc <- PEcAn.utils::read.output(ncfiles = ncfile, dataframe = TRUE) |>
  as_tibble()
nc <- ncdf4::nc_open(ncfile)
names(nc$var)
time <- ncdf4::ncvar_get(nc, "time")
