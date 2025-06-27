plot_mixing <- function(forecast_df, eval_depths = 'min/max', use_density = TRUE, threshold = 0.1) {
  
  # Labels for plot
  my_breaks <- lubridate::with_tz(seq(min(forecast_df$datetime), max(forecast_df$datetime), by = "1 day"),"America/New_York")
  my_label <- lubridate::with_tz(seq(lubridate::as_datetime(forecast_df$reference_datetime)[1], max(forecast_df$datetime), by = "5 days"), "America/New_York")
  my_labels <- as.character(my_breaks)
  my_labels[which(!(my_breaks %in% my_label))] <- " "
  
  my_labels <- as.Date(my_labels, format = "%Y-%m-%d")
  my_labels <- as.character(my_labels)
  my_labels[is.na(my_labels)] <- " "
  
  # which depths should be evaluated to determine mixing
  if (!is.numeric(eval_depths)) {
    # extracts the maximum and minimum in the forecast
    max_depth <- forecast_df |>
      filter(variable == "temperature") |>
      select(datetime, parameter, depth, variable, prediction) |>
      mutate(is_na = ifelse(is.na(prediction), 1, 0)) |>
      group_by(depth) |>
      summarize(num_na = sum(is_na), .groups = "drop") |>
      filter(num_na == 0) |>
      summarize(max = max(depth)) |>
      pull(max)
    
    min_depth <- min(forecast_df$depth, na.rm = T)
  } else {
    # or uses the user specified values
    max_depth <- max(eval_depths)
    min_depth <- min(eval_depths)
  }
  
  # if use_density is false uses a temperature difference
  if (use_density == FALSE) {
    message(paste0('using a ', threshold, ' C temperature difference to define mixing'))
    temp_forecast <- forecast_df |>
      filter(depth %in% c(max_depth, min_depth),
             datetime >= reference_datetime) |>
      pivot_wider(names_from = depth, names_prefix = 'wtr_', values_from = prediction)
    
    colnames(temp_forecast)[which(colnames(temp_forecast) == paste0('wtr_', min_depth))] <- 'min_depth'
    colnames(temp_forecast)[which(colnames(temp_forecast) == paste0('wtr_', max_depth))] <- 'max_depth'
    
    temp_forecast |>
      mutate(mixed = ifelse((min_depth - max_depth) < threshold,
                            1, 0)) |>
      group_by(datetime) |>
      summarise(percent_mix = 100*(sum(mixed)/n())) |>
      ggplot(aes(datetime, y=percent_mix)) +
      geom_line() +
      scale_x_continuous(breaks = my_breaks, labels = my_labels) +
      scale_y_continuous(limits = c(0,100)) +
      labs(y = '% chance of lake\nmixing') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.2)) +
      theme(#text = element_text(size = 20),
            #plot.caption = element_text(size = 12),
            panel.grid.minor = element_blank()) +
      labs(caption = "Disclaimer: Experimental Forecast")
  }
  
  
  if (use_density == TRUE) {
    message(paste0('using a ', threshold, ' kg/m3 density difference to define mixing'))
    
    temp_forecast <- forecast_df |>
      filter(depth %in% c(max_depth, min_depth),
             datetime >= reference_datetime) |>
      pivot_wider(names_from = depth, names_prefix = 'wtr_', values_from = prediction)# %>% na.omit()
    
    colnames(temp_forecast)[which(colnames(temp_forecast) == paste0('wtr_', min_depth))] <- 'min_depth'
    colnames(temp_forecast)[which(colnames(temp_forecast) == paste0('wtr_', max_depth))] <- 'max_depth'
    
    temp_forecast |>
      mutate(min_depth = rLakeAnalyzer::water.density(min_depth),
             max_depth = rLakeAnalyzer::water.density(max_depth),
             mixed = ifelse((max_depth - min_depth) < threshold,
                            1, 0)) |>
      group_by(datetime) |>
      summarise(percent_mix = 100*(sum(mixed)/n())) |>
      ggplot(aes(datetime, y=percent_mix)) +
      geom_line() +
      scale_x_continuous(breaks = my_breaks, labels = my_labels) +
      scale_y_continuous(limits = c(0,100)) +
      labs(y = '% chance of lake\nmixing') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.2)) +
      theme(#text = element_text(size = 20),
            #plot.caption = element_text(size = 12),
            panel.grid.minor = element_blank()) +
      labs(caption = "Disclaimer: Experimental Forecast")
  }
  
  
  
}

