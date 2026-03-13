#' ---
#' title: "Example workflow generating SIPNET event files from CIMIS and CHIRPS data"
#' author: "Alexey N. Shiklomanov"
#' ---

library(targets)

devtools::document("modules/data.land")
devtools::install("modules/data.land", upgrade = FALSE)
devtools::reload("modules/data.land")

targets_file <- here::here("_targets.R")
targets_store <- here::here("_targets/")
tar_config_set(
  script = targets_file,
  store = targets_store
)

#' Write the targets pipeline script to _targets.R in this directory.
tar_script(
  code = {
    library(targets)
    library(tarchetypes)

    # -------------------------------------------------------------------------
    # Helper functions
    # -------------------------------------------------------------------------

    #' Calculate effective available water capacity (mm) for a soil profile
    #' clipped to a given rooting depth.
    calc_effective_awc <- function(
      hzdept_r_cm,
      hzdepb_r_cm,
      awc_r,
      rooting_depth_cm
    ) {
      effective_top    <- pmin(hzdept_r_cm, rooting_depth_cm)
      effective_bottom <- pmin(hzdepb_r_cm, rooting_depth_cm)
      thickness_cm     <- pmax(0, effective_bottom - effective_top)
      # awc_r is cm water / cm soil; multiply by thickness -> cm water -> mm water
      sum(awc_r * thickness_cm, na.rm = TRUE) * 10
    }

    #' Average ETc and WHC across multi-crop parcels (double-cropping hack).
    resolve_multicrop <- function(etc_data, id_col = "id", date_col = "date") {
      id_sym   <- rlang::sym(id_col)
      date_sym <- rlang::sym(date_col)

      multicrop_counts <- etc_data |>
        dplyr::add_count(!!id_sym, !!date_sym, name = "n") |>
        dplyr::filter(.data$n > 1) |>
        dplyr::summarize(
          n_multicrop = dplyr::n_distinct(!!id_sym, !!date_sym),
          .groups = "drop"
        )

      if (multicrop_counts$n_multicrop > 0) {
        message(
          "Multi-crop parcels: ",
          multicrop_counts$n_multicrop,
          " date-parcel combinations have multiple crops. Averaging ETc and WHC values."
        )
      }

      etc_data |>
        dplyr::group_by(!!id_sym, !!date_sym) |>
        dplyr::summarize(
          etc_mm_day   = mean(.data$etc_mm_day,   na.rm = TRUE),
          whc_min_frac = mean(.data$whc_min_frac, na.rm = TRUE),
          whc_mm       = mean(.data$whc_mm,       na.rm = TRUE),
          .groups = "drop"
        )
    }

    # -------------------------------------------------------------------------
    # Package options
    # -------------------------------------------------------------------------

    tar_option_set(packages = c("ggplot2", "rlang"))

    # -------------------------------------------------------------------------
    # Pipeline
    # -------------------------------------------------------------------------

    list(

      # --- Input paths

      tar_target(design_points_path, path.expand(Sys.getenv("DESIGN_POINTS"))),
      tar_target(cimis_eto_cog_path, path.expand(Sys.getenv("CIMIS_ETO_COG"))),
      tar_target(parcels_path,       path.expand(Sys.getenv("LANDIQ_PARCELS"))),
      tar_target(crops_path,         path.expand(Sys.getenv("LANDIQ_CROPS"))),
      tar_target(mslsp_path,         path.expand(Sys.getenv("LANDIQ_TIMESERIES"))),
      tar_target(event_output_dir,   path.expand(Sys.getenv("EVENT_OUTPUT_DIR"))),

      tar_target(validated_paths, {
        stopifnot(
          file.exists(design_points_path),
          dir.exists(cimis_eto_cog_path),
          file.exists(parcels_path),
          file.exists(crops_path),
          dir.exists(mslsp_path),
          length(list.files(mslsp_path, "\\.parquet")) == 7
        )
        dir.create(event_output_dir, showWarnings = FALSE, recursive = TRUE)
        TRUE
      }),

      tar_target(
        design_points,
        readr::read_csv(design_points_path, show_col_types = FALSE) |>
          # Remove duplicate IDs
          dplyr::slice(1, .by = "id")
      ),

      tar_target(
        dp_with_parcels,
        PEcAn.data.land::get_landiq_parcel_ids(design_points, parcels_path) |>
          dplyr::mutate(parcel_id = as.character(parcel_id))
      ),

      tar_target(
        dp_with_phenology,
        PEcAn.data.land::mslsp_to_canopycover(
          mslsp_path,
          parcel_ids = unique(dp_with_parcels[["parcel_id"]])
        ) |>
          dplyr::mutate(
            landiq_SUBCLASS = as.integer(.data$landiq_SUBCLASS)
          ) |>
          dplyr::inner_join(dp_with_parcels, by = "parcel_id") |>
          dplyr::select(-dplyr::starts_with("UniqueID_"))
      ),

      # --- LandIQ crop data --------------------------------------------------

      tar_target(
        design_point_crops,
        {
          #' NOTE: Some LandIQ classes/subclasses map onto multiple BISM crop types.
          #' HACK: select just the first crop per class/subclass group.
          bism_crop_unique <- PEcAn.data.land::bism_kc_by_crop |>
            dplyr::distinct(.data$landiq_class, .data$landiq_subclass, .data$crop_name) |>
            dplyr::slice(1, .by = c("landiq_class", "landiq_subclass"))

          dp_crops <- dp_with_phenology |>
            dplyr::left_join(
              bism_crop_unique,
              by = c(
                "landiq_CLASS" = "landiq_class",
                "landiq_SUBCLASS" = "landiq_subclass"
              )
            )

          missing_crops <- dp_crops |> dplyr::filter(is.na(crop_name))
          if (nrow(missing_crops) > 0) {
            missing_crop_strs <- missing_crops |>
              dplyr::distinct(.data$landiq_CLASS, .data$landiq_SUBCLASS) |>
              dplyr::mutate(
                string = glue::glue(
                  "CLASS: {.data$landiq_CLASS} ",
                  "SUBCLASS: {.data$landiq_SUBCLASS}"
                )
              ) |>
              dplyr::pull(.data$string)
            warning(
              "Skipping ", nrow(missing_crops),
              " rows with no matching BIS crop. Relevant pairs are: [",
              paste(missing_crop_strs, collapse = "; "), "]"
            )
          }
          dp_crops |>
            dplyr::filter(!is.na(.data$crop_name)) |>
            dplyr::left_join(
              PEcAn.data.land::crop_whc |>
                dplyr::select("crop_name", "whc_min_frac", "rooting_depth_m"),
              by = "crop_name"
            )
        }
      ),

      # --- SSURGO soil data --------------------------------------------------

      tar_target(
        mukeys_list,
        {
          design_points_sf <- design_points |> 
            dplyr::distinct(id, lon, lat)
          purrr::map2(
            design_points_sf$lon,
            design_points_sf$lat,
            ~ PEcAn.data.land::ssurgo_mukeys_point(
              point = c(.x, .y),
              distance = 10
            )
          )
        }
      ),

      tar_target(
        soil_raw,
        PEcAn.data.land::gSSURGO.Query(
          mukeys = unique(unlist(mukeys_list)),
          fields = c("chorizon.awc_r", "chorizon.hzdept_r", "chorizon.hzdepb_r")
        )
      ),

      tar_target(
        soil_dominant,
        soil_raw |>
          dplyr::filter(cokey == cokey[which.max(comppct_r)], .by = "mukey")
      ),

      tar_target(
        dp_with_whc,
        design_point_crops |>
          dplyr::mutate(
            mukey = mukeys_list[match(id, design_points$id)]
          ) |>
          tidyr::unnest(mukey) |>
          dplyr::mutate(mukey = as.numeric(mukey)) |>
          dplyr::left_join(
            soil_dominant,
            by = "mukey",
            relationship = "many-to-many"
          ) |>
          dplyr::summarize(
            whc_mm = calc_effective_awc(
              hzdept_r, hzdepb_r, awc_r,
              rooting_depth_cm = rooting_depth_m[[1]] * 100
            ),
            .by = c("id", "parcel_id", "date", "crop_name", "whc_min_frac")
          ) |>
          dplyr::mutate(
            whc_mm = dplyr::if_else(whc_mm > 0, whc_mm, 500, missing = 500)
          )
      ),

      # --- Remote data extractions (slow; most benefit from caching) ---------

      tar_target(
        precip_et_dates,
        with(design_point_crops, seq(min(date), max(date), by = "1 day"))
      ),

      tar_target(
        etref,
        design_point_crops |>
          PEcAn.data.land::extract_cimis_dates(
            precip_et_dates,
            cimis_eto_cog_path,
            download_missing = TRUE,
            .progress = TRUE
          )
      ),

      tar_target(
        precip,
        PEcAn.data.land::extract_chirps_remote(design_points, precip_et_dates)
      ),


      # --- ETc and water balance ---------------------------------------------

      tar_target(
        dp_with_eto,
        dp_with_whc |>
          dplyr::left_join(
            etref |> dplyr::select("id", "date", "etref_mm_day"),
            by = c("id", "date")
          )
      ),

      tar_target(
        dp_with_etc,
        dp_with_eto |>
          dplyr::mutate(
            etc_mm_day = eto_to_etc_bism(
              eto       = etref_mm_day,
              crop_name = crop_name[[1]],
              date      = date
            ),
            .by = "crop_name"
          ) |>
          dplyr::select(
            dplyr::any_of(c("id", "parcel_id", "lat", "lon")),
            "date", "etc_mm_day", "whc_min_frac", "whc_mm"
          ) |>
          resolve_multicrop()
      ),

      tar_target(
        dp_crops_all,
        dp_with_etc |>
          dplyr::inner_join(precip, by = c("id", "date")) |>
          dplyr::select(
            "id", "lat", "lon", "date",
            "etc_mm_day", "precip_mm_day", "whc_min_frac", "whc_mm"
          )
      ),

      tar_target(
        dpwb,
        apply_water_balance(dp_crops_all, "id")
      ),

      # --- Diagnostics -------------------------------------------------------

      tar_target(
        etc_summary,
        dp_crops_all |>
          dplyr::summarize(
            etc_min  = min(.data$etc_mm_day,  na.rm = TRUE),
            etc_max  = max(.data$etc_mm_day,  na.rm = TRUE),
            etc_mean = mean(.data$etc_mm_day, na.rm = TRUE),
            .by = "id"
          )
      ),

      tar_target(
        wb_summary,
        dpwb |>
          dplyr::group_by(.data$id) |>
          dplyr::summarize(
            irr_total    = sum(.data$irr,   na.rm = TRUE),
            irr_max      = max(.data$irr,   na.rm = TRUE),
            irr_mean     = mean(.data$irr,  na.rm = TRUE),
            runoff_total = sum(.data$runoff, na.rm = TRUE),
            W_t_min      = min(.data$W_t,   na.rm = TRUE),
            W_t_max      = max(.data$W_t,   na.rm = TRUE),
            .groups = "drop"
          ) |>
          (\(x) {
            print(x)
            if (any(x$irr_max < 0))  warning("Negative irrigation values detected!")
            else                      message("Irrigation values are non-negative")
            if (any(x$W_t_min < 0))  warning("Negative soil water values detected!")
            else                      message("Soil water values are non-negative")
            x
          })()
      ),

      tar_target(
        monthly_irr,
        dpwb |>
          dplyr::mutate(month = lubridate::month(.data$date)) |>
          dplyr::group_by(.data$month) |>
          dplyr::summarize(irr_mean = mean(.data$irr, na.rm = TRUE), .groups = "drop") |>
          (\(x) { print(x); x })()
      ),

      # --- Plot (saved as PNG) -----------------------------------------------

      tar_target(
        irrigation_plot, {
          p <- dpwb |>
            ggplot2::ggplot() +
            ggplot2::aes(x = date, y = irr, color = id) +
            ggplot2::geom_line() +
            ggplot2::labs(
              title = "Irrigation Requirements by Site",
              y     = "Irrigation (mm/day)"
            )
          path <- file.path(event_output_dir, "irrigation_plot.png")
          ggplot2::ggsave(path, p, width = 10, height = 6)
          path
        },
        format = "file"
      ),

      # --- Write SIPNET event files ------------------------------------------

      tar_target(
        event_files, {
          dpwb |>
            dplyr::group_nest(.data$id) |>
            dplyr::mutate(
              fname = purrr::map2(
                id, data,
                \(site_id, dat) {
                  readr::write_delim(
                    create_event_file(dat),
                    file.path(
                      event_output_dir,
                      glue::glue("{site_id}_events.txt")
                    ),
                    delim      = " ",
                    col_names  = FALSE
                  )
                }
              )
            )
          list.files(event_output_dir, full.names = TRUE,
                     pattern = "_events\\.txt$")
        },
        format = "file"
      )

    )
  },
  ask = FALSE
)

#' Run the pipeline. Targets that are already up-to-date will be skipped.
# tar_make()
# tar_invalidate(dp_with_crops)
tar_make(c(precip))

if (interactive()) {
  # tar_load(c("design_points", "dp_with_crops", "phenology"))
  tar_load_everything()
}
