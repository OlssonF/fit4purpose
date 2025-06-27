#--------------------------------------#
## Project: fit4purpose in ecology
## Script purpose: generate a figure to compare the forecasting visualisations used in different water temperature forecasting applications
## Date: 2025-06-27
## Author: Freya Olsson (contributions to original visualisations - RQT, ADD, CCC,...)
#--------------------------------------#

library(arrow)
library(tidyverse)
library(lubridate)
library(rLakeAnalyzer)
library(ggpubr)

visualise_date <- as_datetime("2024-10-01")

# First visualisation ------------------
## Percent chance of mixing - FLARE water temperature at FCR
source('R/plot_mixing.R')
s3_fcre <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3", 
                            endpoint_override = "amnh1.osn.mghpcc.org", anonymous = TRUE)

# chance of being mixed
fcre_forecast <- arrow::open_dataset(s3_fcre, unify_schemas = FALSE) |> 
  filter(variable == "Temp_C_mean",
         reference_datetime == visualise_date) |> 
  dplyr::collect() 

p1 <-plot_mixing(forecast_df = fcre_forecast, eval_depths = c(1, 8), use_density = T, threshold = 0.1)

# Second visualisation -----------------
## Water recreation - FLARE water temperature at SUNP
s3_sunp <- arrow::s3_bucket('bio230121-bucket01/flare/scores/parquet/site_id=sunp/model_id=glm_flare_v3',
                                      endpoint_override = 'amnh1.osn.mghpcc.org',
                                      anonymous = TRUE)

sunp_vis_date <- as_datetime(Sys.Date() - days(1))
sunp_scores <- arrow::open_dataset(s3_sunp) |>
  filter(reference_datetime == sunp_vis_date,
         variable == 'temperature',
         depth == 0.1) |>
  collect() |>
  filter(datetime < (sunp_vis_date + lubridate::days(8))) |>
  mutate(date = as.Date(datetime)) |> 
  filter(date >= sunp_vis_date,
         date < (sunp_vis_date + lubridate::days(7)))


# can only run on today!
met_forecast_sunp <- readr::read_csv('https://api.open-meteo.com/v1/forecast?latitude=43.39102&longitude=-72.053627&daily=weather_code,temperature_2m_max,temperature_2m_min&timezone=America%2FNew_York&models=gfs_seamless&format=csv') |>
  rename(date = latitude, code = longitude, airtemp_min = utc_offset_seconds, airtemp_max = elevation) |>
  select(date, code, airtemp_min, airtemp_max) |>
  slice(3:9) 

p2 <- plot_sunp(df_scores = sunp_scores, df_weather = met_forecast_sunp)

# Third visualisation ------------------
## Intermodel comparison - EFI-NEON challenge 

source('R/visualisation_comparison.R')
s3_neon <- arrow::s3_bucket(bucket = "bio230014-bucket01/challenges/scores/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature", 
                            endpoint_override = "sdsc.osn.xsede.org", anonymous = TRUE)

neon_forecast <- arrow::open_dataset(s3_neon, unify_schemas = FALSE) |> 
  filter(site_id == "BARC",
         reference_datetime == visualise_date) |> 
  collect() 

p3 <- plot_neon_forecast(neon_forecast, show_obs = T)
plot_neon_scores(neon_forecast)


# Combine
ggarrange(p1, p3, p2, nrow = 2, ncol = 2, labels = 'AUTO')
