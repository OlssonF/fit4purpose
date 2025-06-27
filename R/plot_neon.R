plot_neon_forecast <- function(df, show_obs = T){
  ggplot(df, aes(x=datetime, y=mean)) +
    geom_ribbon(aes(ymax=quantile97.5, ymin = quantile02.5, fill = model_id), alpha = 0.2) +
    geom_line(aes(colour = model_id)) +
    theme_bw() +
    geom_point(aes(y=observation), alpha = 0.6) +
    labs(y= 'prediction') +
    scale_colour_discrete(name = 'Model ID') +
    scale_fill_discrete(name = 'Model ID') +
    scale_x_datetime(date_labels = "%d %b")
  
}


plot_neon_scores <- function(df) {
  ggplot(df, aes(x=datetime, y=crps)) +
    geom_line(aes(colour = model_id)) +
    theme_bw() +
    scale_colour_discrete(name = 'Model ID')
}
