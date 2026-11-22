library(tidyverse)

add_new_sample_and_sector <- function(reg_results, sector_name, sample_id = NULL, sample_label, overall_sample_label, vars_to_plot_start) {
  if (is.null(sample_id)) {
    results <- reg_results$coeftable
  } else {
    results <- reg_results[[sample_id]]$coeftable
  }
  
  data.frame(results) %>% 
    mutate(vars = row.names(.), Sector = sector_name, !!overall_sample_label := sample_label) %>% 
    filter(str_starts(vars, vars_to_plot_start))
}

plot_aqi_bin_by_sample <- function(sector_label_1, sector_results_1, sector_label_2 = NULL, sector_results_2 = NULL, sample_ix_map = NULL, overall_sample_label = "Sample", vars_to_plot_start = 'adj_pm_smoke_bin', split_by_sample = TRUE) {
  results_dfm <- data.frame()
  
  if (is.null(sample_ix_map)) {
    get_sector_results <- function(sector_results, sector_label) {
      add_new_sample_and_sector(sector_results, sector_label, sample_id = NULL, sample_label = overall_sample_label, overall_sample_label = overall_sample_label, vars_to_plot_start = vars_to_plot_start)
    }
    
    if (!is.null(sector_label_2) && !is.null(sector_results_2)) {
      results_dfm <- bind_rows(
        get_sector_results(sector_results_1, sector_label_1),
        get_sector_results(sector_results_2, sector_label_2)
      )
    } else {
      results_dfm <- bind_rows(
        get_sector_results(sector_results_1, sector_label_1)
      )
    }
  } else {
    for (sample_label in names(sample_ix_map)) {
      sample_ix <- sample_ix_map[[sample_label]]
      
      get_sector_results <- function(sector_results, sector_label) {
        add_new_sample_and_sector(sector_results, sector_label, sample_id = sample_ix, sample_label = sample_label, overall_sample_label = overall_sample_label, vars_to_plot_start = vars_to_plot_start)
      }
      
      if (!is.null(sector_label_2) && !is.null(sector_results_2)) {
        results_dfm <- results_dfm %>% 
          bind_rows(
            get_sector_results(sector_results_1, sector_label_1),
            get_sector_results(sector_results_2, sector_label_2)
          )
      } else {
        results_dfm <- results_dfm %>% 
          bind_rows(
            get_sector_results(sector_results_1, sector_label_1)
          )
      }
    }
  }
  
  row.names(results_dfm) <- NULL
  
  if (split_by_sample) {
    shape_var <- "Sector"
    split_var <- overall_sample_label
  } else {
    shape_var <- overall_sample_label
    split_var <- "Sector"
  }
  
  plot_data <- results_dfm %>%
    mutate(
      low_ci = Estimate - 1.96 * Std..Error,
      high_ci = Estimate + 1.96 * Std..Error,
      bin = str_sub(vars, start = -7)
    )
  print(plot_data)
  base_plot <- ggplot(plot_data, aes(x = bin, y = Estimate, shape = get(shape_var), color = bin, ymin = low_ci, ymax = high_ci)) +
    geom_hline(yintercept = 0, color = "darkgrey") +
    geom_errorbar(position = position_dodge(width = 0.5), width = 0.2, linewidth=1) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    facet_wrap(as.formula(paste("~", split_var))) +
    labs(x = "Smoke PM Bin") +
    scale_color_manual(
      "Smoke PM AQI\nBin equivalent",
      values = c('forestgreen', 'gold', 'orange', 'red', 'purple', 'maroon'),
      labels = c("Good", "Moderate", "Unhealthy for \nSensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
    ) +
    scale_y_continuous("Marginal Change in Visits", labels = scales::label_percent()) +
    theme_minimal(base_size = 18) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    guides(color = "none")  # Always hide color legend
  
  # Conditionally add/remove shape legend
  if (length(unique(results_dfm[[shape_var]])) > 1) {
    base_plot + scale_shape(name = shape_var)
  } else {
    base_plot + guides(shape = "none")
  }
}


## example usage: 
#plot_aqi_bin_by_sample("All Sectors", est_visits_log_weekend, "Deferrable Sectors", est_visits_def_log_weekend, sample_ix_map=c("Weekend"=3, "Weekday"=2), overall_sample_label="Sample", split_by_sample=F)


make_event_study <- function(model_coeffs, low=-10, high=30, color='black'){
  coeffs <- data.frame(model_coeffs)
  
  n_other_vars <- nrow(coeffs) - (high - low + 1)
  print(n_other_vars)
  coeffs$t <- c(low:0, 1:high, rep(NA, n_other_vars))
  rescaler <- 0 - filter(coeffs, t==-1)$Estimate
  coeffs <- coeffs %>% mutate(
    Std..Error=if_else(t==-1, 0, Std..Error),
    Estimate = Estimate + rescaler,
    bar_width=if_else(t==-1, 0, 0.5)
  )
  colors <- c("Day Before Smoke Exposure" = "darkgrey", "Event Study Coefficients"=color)
  ggplot(coeffs, aes(x=t, y=Estimate, ymin=Estimate-1.96*Std..Error, ymax=Estimate+1.96*Std..Error))+
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1, color="darkgrey", lty=2) +
    geom_point(color=color, size=3) +
    geom_errorbar(color=color, aes(width=bar_width), linewidth=1) +
    theme_minimal(base_size = 18)
}


## example usage: 

