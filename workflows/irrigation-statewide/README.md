# Statewide irrigation events workflow

This generates PEcAn event files for irrigation events across all of California.
The spatial unit is harmonized LandIQ parcels.

The workflow uses `targets` for reproducibility, scalability, and incremental execution.
The pipeline is defined in `_targets.R` and can be executed via the `run-pipeline.R` script (which is just a thin wrapper around `library(targets); tar_make()`).

# Setup

The workflow requires the following environment variables to be set:

- `TAR_CONFIG` --- path to the `_targets.yaml` config file in this directory.
- `N_PARCELS` --- the number of parcels to process (sampled at random) or `all` to run for all LandIQ parcels.
- `BATCH_SIZE` --- number of parcels per "batch". Each batch gets its own target. Note that having too many small batches creates a lot of overhead.
- `N_REMOTE_WORKERS` --- number of remote workers (SGE jobs) to spawn for execution 
- `EXEC_TYPE` --- execution type. Either `local` (run on current machine, with `NSLOTS` parallel processes) or `cluster` (to run using SGE jobs)

- `LANDIQ_CROPS` --- path to harmonized LandIQ crops file (`crops_all_years.parq`)
- `LANDIQ_TIMESERIES` --- path to HLS-based phenology (MSLSP) parquet files

- `EVENT_OUTPUT_DIR` --- output directory where final event files will be written. If it doesn't exist, it will be created.
- `EVENT_FILENAME` --- name of event file to be created. Should have `.parquet` extension. It will be placed in `EVENT_OUTPUT_DIR`

- `CHIRPS_PRECIP` --- path to pre-extracted CHIRPS precipitation data (folder containing parquet files)
- `CIMIS_ETREF` --- path to pre-extracted CIMIS evapotranspiration data (folder containing parquet files)
- `SSURGO_WEIGHTS` --- path to pre-computed SSURGO weights for LandIQ parcels (single parquet file)
- `SSURGO_GDB` --- path to complete SSURGO geodatabase (geodatabase; folder with `.gdb` extension)

A good way to set these is via a project-local `.Renviron` file that looks like this:

```
TAR_CONFIG=modules/data.land/inst/irrigation-statewide/_targets.yaml

LANDIQ_PARCELS=/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/parcels.gpkg
LANDIQ_CROPS=/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/crops_all_years.parq
LANDIQ_TIMESERIES=/projectnb/dietzelab/ccmmf/management/phenology/matched_landiq_mslsp_v4.1
EVENT_OUTPUT_DIR=/projectnb/dietzelab/ccmmf/usr/ashiklom/event-outputs

CHIRPS_PRECIP=/projectnb/dietzelab/ccmmf/data/chirps-extracted
CIMIS_ETREF=/projectnb/dietzelab/ccmmf/data/cimis-extracted
SSURGO_WEIGHTS=/projectnb/dietzelab/ccmmf/data_raw/ssurgo/ssurgo-weights.parquet
SSURGO_GDB=/projectnb/dietzelab/ccmmf/data_raw/ssurgo/gSSURGO_CA.gdb
```

Use R commands like `Sys.getenv("TAR_CONFIG")` from inside your R session to confirm these variables are set correctly.

# Execution

Assuming the variables above are set, you can run the pipeline with just `Rscript -e 'targets::tar_make()'`.
