library(dplyr)
library(weather) #  devtools::install_github("dreamRs/weather")
library(ggplot2)
library(patchwork)
library(cowplot)


plot_sunp <- function(df_scores, df_weather) {
  
  #0	Clear sky
  #1, 2, 3	Mainly clear, partly cloudy, and overcast
  #45, 48	Fog and depositing rime fog
  #51, 53, 55	Drizzle: Light, moderate, and dense intensity
  #56, 57	Freezing Drizzle: Light and dense intensity
  #61, 63, 65	Rain: Slight, moderate and heavy intensity
  #66, 67	Freezing Rain: Light and heavy intensity
  #71, 73, 75	Snow fall: Slight, moderate, and heavy intensity
  #77	Snow grains
  #80, 81, 82	Rain showers: Slight, moderate, and violent
  #85, 86	Snow showers slight and heavy
  #95 *	Thunderstorm: Slight or moderate
  #96, 99 *	Thunderstorm with slight and heavy hail
  
  weather_description <- data.frame(code = as.character(c(0,1,2,3,45,48,51,53,55,56,57,61,63,65,66,67,71,73,75,77,80,81,82,85,86,95,96,99)),
                                    description = c('day-sunny', #0
                                                    'day-sunny-overcast', #1
                                                    'day-cloudy', #2
                                                    'cloudy', #3
                                                    'day-fog', #45
                                                    'fog', #48
                                                    'day-showers', #51
                                                    'rain', #53
                                                    'rain', #55
                                                    'day-sleet', #56
                                                    'sleet', #57
                                                    'day-rain', #61
                                                    'rain', #63
                                                    'rain', #65
                                                    'day-sleet', #66
                                                    'sleet', #67
                                                    'day-snow-wind', #71
                                                    'snow', #73
                                                    'snow', #75
                                                    'snow', #77
                                                    'day-rain', #80
                                                    'rain', #81
                                                    'rain', #82
                                                    'day-snow-wind', #85
                                                    'snow', #86
                                                    'thunderstorm', #95
                                                    'thunderstorm', #96,
                                                    'thunderstorm' #99
                                    ))
  
  df_weather <- df_weather |>
    left_join(weather_description, by = 'code') |>
    mutate(description = ifelse(is.na(description),'na',description),
           airtemp_min = (round(((as.numeric(airtemp_min)) * (9/5) + 32))),
           airtemp_max = (round(((as.numeric(airtemp_max)) * (9/5) + 32))))
  
  plotting_frame <- data.frame(height = seq.int(1,100), width = seq.int(1,400))
  
  water_temp_range <- df_scores |>
    select(date, quantile02.5, quantile97.5) |>
    mutate(watertemp_text = paste0(format(round(quantile02.5, digits = 1), nsmall = 1), ' - ', format(round(quantile97.5, digits = 1), nsmall = 1))) |>
    mutate(height = 36,
           width = c(53, 108, 162, 215, 270, 323, 377))
  
  water_temp_median <- df_scores |>
    select(date, median) |>
    mutate(median = format(round(median, digits = 1), nsmall = 1)) |>
    mutate(height = 40,
           width = c(53, 108, 162, 215, 270, 323, 377))
  
  air_temps <- df_weather |>
    mutate(airtemp_text = paste0(round(airtemp_min), ' - ', round(airtemp_max))) |>
    select(date, airtemp_text) |>
    mutate(height = 57,
           width = c(55, 110, 162, 217, 270, 324, 379))
  
  
  
  weather_icons <- df_weather |>
    select(date, description) |>
    mutate(#height = 62, ## USE THESE FOR RUNNING LOCALLY - NOT ON GH
      #width = c(39, 95, 147, 203, 254, 308, 362), ## USE THESE FOR RUNNING LOCALLY - NOT ON GH
      height = 67,
      width = c(55, 110, 162, 217, 270, 324, 379))
  
  
  day_df <- weather_icons |>
    mutate(weekday = weekdays(as.Date(date))) |>
    select(weekday, date) |>
    mutate(height = 22,
           width = c(55, 108, 162, 217, 270, 325, 378))
  
  day_df$weekday[1] <- 'Today'
  
  date_df <- day_df |>
    mutate(month = lubridate::month(as.Date(date, format="%Y-%m-%d")),
           month_abbr = month.abb[month],
           #month_name = months(as.Date(date, format="%Y-%m-%d")),
           day = lubridate::day(as.Date(date, format="%Y-%m-%d")),
           year = lubridate::year(as.Date(date, format="%Y-%m-%d"))) |>
    mutate(date_value = paste(day, month_abbr, year)) |>
    select(date_value) |>
    mutate(height = 18,
           width = c(55, 108, 162, 217, 270, 325, 379))
  
  img_overlay <- ggplot(plotting_frame, aes(width, height)) +
    geom_weather(data = weather_icons, aes(width, height, weather = description), size = 8) +
    geom_text(data = water_temp_range, aes(x = width, y = height, label = watertemp_text), size = 1.6, fontface = 'bold', family = 'Ariel') +
    geom_text(data = water_temp_median, aes(x = width, y = height, label = median), size = 2.2, fontface = 'bold', family = 'Ariel') +
    geom_text(data = air_temps, aes(x = width, y = height, label = airtemp_text), size = 2.2, fontface = 'bold', family = 'Ariel') +
    geom_text(data = day_df, aes(x = width, y = height, label = weekday), size = 2, fontface = 'bold', family = 'Ariel') +
    geom_text(data = date_df, aes(x = width, y = height, label = date_value), size = 1.5, fontface = 'bold', family = 'Ariel') +
    xlim(1,400) +
    ylim(1,100) +
    theme_cowplot() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  #img_overlay
  
  sunp_plot <- ggdraw() +
    draw_image("SunapeeVis5.jpeg", scale = 0.95, width = 1, height = 1, x = -0.01) +
    draw_plot(img_overlay) 
  sunp_plot
}
