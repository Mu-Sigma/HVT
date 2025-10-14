#' @name mcmc_plots
#' @title Consolidated MCMC Plots Function
#' @description Creates comprehensive plots for HVT-MSM simulation results with minimal code duplication.
#' @param simulation_results Data frame with simulation results
#' @param centroid_data Data frame with centroid information
#' @param centroid_2d_points Data frame with 2D coordinates
#' @param actual_data Data frame with actual values
#' @param state_time_data Data frame with state-time information
#' @param forecast_type Type of forecast ("ex-post" or "ex-ante")
#' @param n_ahead_ante Number of ahead steps for ex-ante
#' @param type Plot type identifier
#' @param raw_dataset Raw dataset for scaling
#' @param show_simulation Whether to show simulation lines
#' @param mae_metric MAE metric to use
#' @param time_column Name of time column
#' @param trainHVT_results HVT training results
#' @return List containing all plots and data frames
#' @author Vishwavani <vishwavani@mu-sigma.com>
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom stats na.omit

mcmc_plots <- function(simulation_results, centroid_data, centroid_2d_points, actual_data, 
                      state_time_data, forecast_type, n_ahead_ante, type, raw_dataset, 
                      show_simulation, mae_metric = mae_metric, time_column, trainHVT_results) {
  
  # Global variables for CRAN warnings
  time <- simulation <- median <- sd <- studentized_residuals <- value <- Cell.ID <- NULL

  # Standardize timestamp function
  standardize_timestamp <- function(data, time_col) {
    sample_time <- na.omit(data[[time_col]])[1]
    if(inherits(sample_time, "POSIXct")) return(data)
    
    formats <- c("%m/%d/%Y", "%m-%d-%Y", "%Y/%m/%d", "%Y-%m-%d", 
                 "%Y-%m-%d %H:%M:%S", "%d/%m/%Y")
    
    for(fmt in formats) {
      tryCatch({
        data[[time_col]] <- as.POSIXct(data[[time_col]], format = fmt)
        if(!all(is.na(data[[time_col]]))) break
      }, error = function(e) {})
    }
    return(data)
  }
  
  # Analyze timestamp characteristics
  analyze_timestamps <- function(data, time_col) {
    data <- standardize_timestamp(data, time_col)
    sorted_times <- sort(data[[time_col]])
    
    changes <- list(
      year = length(unique(format(sorted_times, "%Y"))),
      month = length(unique(format(sorted_times, "%Y-%m"))),
      day = length(unique(format(sorted_times, "%Y-%m-%d"))),
      hour = length(unique(format(sorted_times, "%Y-%m-%d %H"))),
      minute = length(unique(format(sorted_times, "%Y-%m-%d %H:%M"))),
      second = length(unique(format(sorted_times, "%Y-%m-%d %H:%M:%S")))
    )
    
    date_format <- if(changes$minute == changes$second) {
      if(changes$hour == changes$minute) {
        if(changes$day == changes$hour) {
          if(changes$month == changes$day) {
            if(changes$year == changes$month) "%Y" else "%Y-%m"
          } else "%Y-%m-%d"
        } else "%Y-%m-%d %H"
      } else "%Y-%m-%d %H:%M"
    } else "%Y-%m-%d %H:%M:%S"
    
    time_diffs <- diff(sorted_times)
    median_diff <- median(time_diffs)
    days_diff <- as.numeric(median_diff, units="days")
    
    interval_type <- dplyr::case_when(
      between(days_diff, 57, 62) ~ "bimonthly",
      between(days_diff, 27, 31) ~ "monthly",
      between(days_diff, 88, 92) ~ "quarterly",
      between(days_diff, 364, 366) ~ "yearly",
      between(days_diff, 6, 8) ~ "weekly",
      between(days_diff, 13, 15) ~ "biweekly",
      days_diff < 1 ~ "intraday",
      days_diff == 1 ~ "daily",
      TRUE ~ "irregular"
    )
    
    total_span <- as.numeric(difftime(max(sorted_times), min(sorted_times), units = "days"))
    
    return(list(
      interval_type = interval_type,
      date_format = date_format,
      total_span = total_span,
      standardized_data = data
    ))
  }
  
  # Get regular timestamp breaks
  get_regular_breaks <- function(data_time, interval_type) {
    data_time <- sort(data_time)
    first_timestamp <- data_time[1]
    max_timestamp <- max(data_time)
    
    if(interval_type == "bimonthly") {
      breaks <- seq(from = first_timestamp, to = max_timestamp, by = "2 months")
      if(length(breaks) > 10) {
        step <- ceiling(length(breaks) / 10)
        indices <- c(1, seq(from = 1 + step, to = length(breaks), by = step))
        breaks <- breaks[indices]
      }
      return(breaks)
    }
    
    interval <- switch(interval_type,
                       "daily" = "1 day", "weekly" = "1 week", "biweekly" = "2 weeks",
                       "monthly" = "1 month", "quarterly" = "3 months", "yearly" = "1 year",
                       "intraday" = "6 hours", "irregular" = "1 month")
    
    breaks <- seq(from = first_timestamp, to = max_timestamp, by = interval)
    
    if(length(breaks) > 20) {
      step <- ceiling(length(breaks) / 20)
      indices <- c(1, seq(from = 1 + step, to = length(breaks), by = step))
      breaks <- breaks[indices]
    }
    
    return(breaks)
  }
  
  # Get axis breaks
  get_axis_breaks <- function(time_analysis) {
    break_interval <- switch(time_analysis$interval_type,
                             "intraday" = if(grepl("%H:%M:%S", time_analysis$date_format)) "1 minute" 
                             else if(grepl("%H:%M", time_analysis$date_format)) "15 minutes" 
                             else "1 hour",
                             "daily" = "1 day", "weekly" = "1 week", "biweekly" = "2 weeks",
                             "monthly" = "1 month", "quarterly" = "3 months", "yearly" = "1 year",
                             "bimonthly" = "2 months",
                             {
                               months_span <- time_analysis$total_span / 30.44
                               if(months_span <= 1) "1 week"
                               else if(months_span <= 3) "2 weeks"
                               else if(months_span <= 6) "1 month"
                               else if(months_span <= 24) "1 month"
                               else if(months_span <= 60) "2 months"
                               else "1 year"
                             })
    
    return(list(break_interval = break_interval, date_format = time_analysis$date_format))
  }
  
  # Common theme
  theme_plot <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 12),
    legend.position = "right",
    legend.key.width = grid::unit(0.5, "cm")
  )
  
  # Integer breaks function
  integer_breaks <- function(n = 5, ...) {
    function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
  }
  
  # Create base plot with common elements
  create_base_plot <- function(plot_data, summary_data, show_simulation, mae_metric, 
                              variable_name = NULL, is_states = FALSE) {
    
    # Color scheme
    color_values <- c("Simulations" = "darkgray", "Median" = "red", "Mean" = "darkgreen", 
                     "Mode" = "#0901FF")
    if(!is_states) color_values["Actual"] <- "black"
    
    plot <- ggplot2::ggplot() +
      # Simulation lines
      {if(show_simulation) ggplot2::geom_line(data = plot_data,
                                             ggplot2::aes(x = time, y = value, group = simulation, 
                                                         colour = "Simulations",
                                                         text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), 
                                             alpha = 0.4, size = 0.4)} +
      # Mode line and points
      ggplot2::geom_line(data = summary_data, ggplot2::aes(x = time, y = mode, colour = "Mode"), 
                        size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
      ggplot2::geom_point(data = summary_data, ggplot2::aes(x = time, y = mode, colour = "Mode",
                                                          text = paste("Time:", time, "<br>Mode", 
                                                                      ifelse(is.null(variable_name), "", paste(variable_name, ":")), mode)), 
                         size = ifelse(mae_metric == "mode", 1.5, 1.0)) +
      # Mean line and points
      ggplot2::geom_line(data = summary_data, ggplot2::aes(x = time, y = mean, colour = "Mean"), 
                        size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
      ggplot2::geom_point(data = summary_data, ggplot2::aes(x = time, y = mean, colour = "Mean",
                                                           text = paste("Time:", time, "<br>Mean", 
                                                                       ifelse(is.null(variable_name), "", paste(variable_name, ":")), mean)), 
                         size = ifelse(mae_metric == "mean", 1.5, 1.0)) +
      # Median line and points
      ggplot2::geom_line(data = summary_data, ggplot2::aes(x = time, y = median, colour = "Median"), 
                        size = ifelse(mae_metric == "median", 1.0, 0.4)) +
      ggplot2::geom_point(data = summary_data, ggplot2::aes(x = time, y = median, colour = "Median",
                                                           text = paste("Time:", time, "<br>Median", 
                                                                       ifelse(is.null(variable_name), "", paste(variable_name, ":")), median)), 
                         size = ifelse(mae_metric == "median", 1.5, 1.0)) +
      ggplot2::scale_colour_manual(values = color_values) +
      ggplot2::theme_minimal() +
      theme_plot
    
    return(plot)
  }
  
  # Add actual data layer
  add_actual_layer <- function(plot, actual_data, variable_name, time_col = "time") {
    actual_col_name <- paste0("actual_", variable_name)
    plot + 
      ggplot2::geom_line(data = actual_data, 
                        ggplot2::aes(x = !!rlang::sym(time_col), y = !!rlang::sym(actual_col_name), 
                                    colour = "Actual"), size = 1.0) +
      ggplot2::geom_point(data = actual_data, 
                         ggplot2::aes(x = !!rlang::sym(time_col), y = !!rlang::sym(actual_col_name), 
                                     colour = "Actual",
                                     text = paste("Time:", !!rlang::sym(time_col), "<br>Actual", variable_name, ":", 
                                                 round(!!rlang::sym(actual_col_name), 4))), 
                         size = 1.5)
  }
  
  # Create residual plot
  create_residual_plot <- function(residuals_df, variable_name = NULL) {
    if(all(residuals_df$residuals == 0)) {
      residuals_df$studentized_residuals <- 0
    }
    
    # Determine the correct time column name
    time_col <- if("time" %in% names(residuals_df)) "time" else "t"
    
    ggplot2::ggplot() +
      ggplot2::geom_line(data = residuals_df, 
                        ggplot2::aes(x = !!rlang::sym(time_col), y = studentized_residuals, color = "Studentized\nResiduals"), 
                        size = 0.8) +
      ggplot2::geom_point(data = residuals_df, 
                         ggplot2::aes(x = !!rlang::sym(time_col), y = studentized_residuals, color = "Studentized\nResiduals",
                                     text = paste("Time:", !!rlang::sym(time_col), "<br>Residuals", 
                                                 ifelse(is.null(variable_name), "", paste(variable_name, ":")), 
                                                 round(studentized_residuals, 4))), 
                         size = 1.0) +
      ggplot2::geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = -1, col = "blue", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 1, col = "blue", linetype = "dashed") +
      ggplot2::scale_color_manual(name = NULL, values = c("Studentized\nResiduals" = "black")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right", legend.key.width = grid::unit(0.5, "cm")) +
      theme_plot
  }
  
  # Apply time scaling to plot
  apply_time_scaling <- function(plot, summary_data) {
    if(inherits(summary_data$time, "POSIXct")) {
      time_analysis <- analyze_timestamps(summary_data, "time")
      axis_settings <- get_axis_breaks(time_analysis)
      all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
      
      plot + 
        ggplot2::scale_x_datetime(
          breaks = all_breaks,
          minor_breaks = summary_data$time,
          date_labels = axis_settings$date_format,
          expand = ggplot2::expansion(mult = 0.02)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      plot + 
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.02)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  }
  
  # Apply plotly layout
  apply_plotly_layout <- function(plot, plot_type, variable_name, height = 400) {
    plotly::ggplotly(plot, tooltip = "text") %>%
      plotly::layout(
        margin = list(r = 150, b = 50, t = 50),
        height = height,
        yaxis = list(title = list(text = ifelse(is.null(variable_name), "States", 
                                              paste0(variable_name, " (raw units)")))),
        xaxis = list(title = list(text = "Timestamps")),
        legend = list(title = list(text = ""))
      )
  }
  
  # Create HTML layout
  create_html_layout <- function(plot1, plot2, type, variable_name, mae_metric, mae) {
    htmltools::tagList(
      htmltools::div(
        style = "display: grid; grid-template-columns: 1fr; max-width: 100%; font-family: Arial, sans-serif;",
        htmltools::div(
          style = "grid-column: 1; width: 100%; text-align: left; font-weight: bold; font-size: 18px; padding-left: 60px; font-family: Arial, sans-serif;",
          paste0(ifelse(is.null(type), "DefaultType", type), 
                 ": Ex-Post Actual ", 
                 ifelse(is.null(variable_name), "Variable", variable_name), 
                 " vs Predicted ", 
                 ifelse(is.null(variable_name), "Variable", variable_name))
        ),
        htmltools::div(style = "grid-column: 1; width: 100%;", plot1),
        htmltools::div(
          style = "grid-column: 1; width: 100%; text-align: left; font-weight: bold; font-size: 18px; padding-left: 60px; font-family: Arial, sans-serif;",
          paste0(ifelse(is.null(type), "DefaultType", type), 
                 ": Ex-Post Studentized Residuals for the ", 
                 ifelse(is.null(mae_metric), "DefaultMetric", mae_metric), " forecast")
        ),
        htmltools::div(
          style = "grid-column: 1; width: 100%; text-align: left; font-size: 14px; padding-left: 60px; font-style: italic;",
          paste("MAE:", ifelse(is.null(mae), "N/A", mae))
        ),
        htmltools::div(style = "grid-column: 1; width: 100%;", plot2)
      )
    )
  }
  
  # Get column names and prepare centroid data
  name_columns <- colnames(centroid_data)
  centroid_data <- centroid_data %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 4)))
  
  # Calculate mean and sd of raw dataset
  raw_dataset_wo_time <- raw_dataset %>% dplyr::select(-(time_column))
  mean_raw <- raw_dataset_wo_time %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.), 4)))
  sd_raw <- raw_dataset_wo_time %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(stats::sd(.), 4)))
  
  # Join dataframes
  centroid_dataframe <- cbind(centroid_data, Cell.ID = centroid_2d_points$Cell.ID)
  
  # Generate predicted dataframes
  generate_predicted_df <- function(coord) {
    centroid_map <- centroid_dataframe %>% dplyr::select(Cell.ID, coord)
    
    replace_with_value <- function(column) {
      centroid_map[[coord]][match(column, centroid_map$Cell.ID)]
    }
    
    sim_df <- simulation_results[,-1] %>%
      dplyr::mutate_all(replace_with_value) %>%
      dplyr::rename_with(~ paste0(., "_", coord), starts_with("Sim_"))
    
    simulation_results %>%
      dplyr::select(time) %>%
      dplyr::bind_cols(sim_df)
  }
  
  predicted_dfs <- lapply(name_columns, generate_predicted_df)
  names(predicted_dfs) <- name_columns
  
  # Scale predicted centroids
  if(trainHVT_results[["model_info"]][["input_parameters"]][["normalize"]]){
    scaled_dfs <- lapply(names(predicted_dfs), function(name) {
      scale_df <- predicted_dfs[[name]] %>%
        dplyr::select(-time) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ (. * sd_raw[1, name]) + mean_raw[1, name])) 
      
      scale_df <- round(scale_df, 4)
      predicted_df <- cbind(time = predicted_dfs[[name]]$time, scale_df)
      return(predicted_df)
    })
  } else {
    scaled_dfs <- predicted_dfs
  }
  names(scaled_dfs) <- names(predicted_dfs)
  
  if(is.numeric(state_time_data[[time_column]])){  
    if (forecast_type == "ex-post") {
      
      test_dataset <- actual_data
      test_data <- state_time_data[state_time_data[[time_column]] %in% simulation_results$time, ]
      
      # Prepare actual data
      actual_raw_dfs <- lapply(name_columns, function(col_name) {
        df <- test_dataset[, c(time_column, col_name)]
        names(df) <- c("time", paste0("actual_", col_name))
        return(df)
      })
      names(actual_raw_dfs) <- name_columns
      
      # Generate all plots for actual vs predicted and residuals
      all_plots <- lapply(name_columns, function(variable_name) {
        
        # Prepare data
        plot_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, mean, median, mode)
        
        predicted_metric <- summary_data[[mae_metric]]
        actual_col_name <- paste0("actual_", variable_name)
        
        # Calculate residuals
        residuals_df <- data.frame(
          t = test_dataset[[time_column]][seq_along(predicted_metric)],
          x = actual_raw_dfs[[variable_name]][[actual_col_name]][seq_along(predicted_metric)],
          predicted_metric
        )
        residuals_df$residuals <- residuals_df$x - residuals_df$predicted_metric
        residuals_sd <- stats::sd(residuals_df$residuals)
        residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
        residuals_df$mape_component <- abs((residuals_df$x - residuals_df$predicted_metric) / residuals_df$x) * 100
        options(scipen = 999)
        # mape <- round(mean(residuals_df$mape_component), 4)  # Commented out as not used
        mae <- round(mean(abs(residuals_df$residuals)), 4)
        
        # Create plots
        p1 <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, variable_name)
        p1 <- add_actual_layer(p1, actual_raw_dfs[[variable_name]], variable_name)
        p1 <- apply_time_scaling(p1, summary_data)
        
        p2 <- create_residual_plot(residuals_df, variable_name)
        p2 <- apply_time_scaling(p2, summary_data)
        
        # Convert to plotly
        p1_plotly <- apply_plotly_layout(p1, type, variable_name, 400)
        p2_plotly <- apply_plotly_layout(p2, type, NULL, 250)
        
        # Create combined layout
        x_plots <- create_html_layout(p1_plotly, p2_plotly, type, variable_name, mae_metric, mae)
        
        list(centroids_plot = x_plots, mae = mae)
      })
      
      # States plot
      plot_data <- simulation_results %>%
        dplyr::select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
      
      summary_data <- simulation_results %>%
        dplyr::select(time, mean, median, mode)
      
      pa <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, NULL, TRUE) +
        ggplot2::geom_line(data = test_data, ggplot2::aes(x = t, y = Cell.ID, color = "Actual States"), size = 1) +
        ggplot2::geom_point(data = test_data, ggplot2::aes(x = t, y = Cell.ID, color = "Actual States", 
                                                          text = paste("Time:", t, "<br>Actual States:", Cell.ID)), 
                           size = 1.5) +
        ggplot2::scale_colour_manual(values = c("Simulations" = "darkgray", "Median" = "red",
                                               "Mean" = "darkgreen", "Actual States" = "black", "Mode" = "#0901FF")) +
        ggplot2::scale_y_continuous(limits = c(1, NA), breaks = integer_breaks()) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "right")
      
      # States residual plot
      predicted_metric <- summary_data[[mae_metric]]
      residuals_df <- data.frame(test_data[seq_along(predicted_metric),], predicted_metric)
      residuals_df$residuals <- (residuals_df$Cell.ID - residuals_df$predicted_metric)
      residuals_sd <- stats::sd(residuals_df$residuals)
      residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
      residuals_df$mape_component <- abs((residuals_df$Cell.ID - residuals_df$predicted_metric) / residuals_df$Cell.ID) * 100
      options(scipen = 999)
      mape <- round(mean(residuals_df$mape_component), 4)
      mae <- round(mean(abs(residuals_df$residuals)), 4)
      
      pb <- create_residual_plot(residuals_df, NULL)
      
      # Convert to plotly
      pa_plotly <- apply_plotly_layout(pa, type, NULL, 400)
      pb_plotly <- apply_plotly_layout(pb, type, NULL, 250)
      
      # Create states layout
      x_plots <- create_html_layout(pa_plotly, pb_plotly, type, "States", mae_metric, mae)
      states_plots <- list(x_plots, mae = mae)
      
      return(list(all_plots, states_plots))
      
    } else {
      # Ex-ante forecasts for numeric time
      all_plots <- lapply(name_columns, function(variable_name) {
        plot_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, mean, median, mode)
        
        p1 <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, variable_name)
        p1 <- apply_time_scaling(p1, summary_data)
        p1 <- p1 + ggplot2::labs(x = "Timestamps", y = paste0(variable_name, " (raw units)"),
                                 title = paste0(type, ": Ex-Ante Predicted ", variable_name), color = " ")
        
        plotly::ggplotly(p1, tooltip = "text")
      })
      
      # States plot for ex-ante
      plot_data <- simulation_results %>%
        dplyr::select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
      summary_data <- simulation_results %>%
        dplyr::select(time, mean, median, mode)
      
      p2 <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, NULL, TRUE) +
        ggplot2::scale_y_continuous(limits = c(1, NA), breaks = integer_breaks()) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "right") +
        ggplot2::labs(x = "Timestamps", y = "States", title = paste0(type, ": Ex-Ante Predicted States"), color = " ")
      
      p2 <- apply_time_scaling(p2, summary_data)
      p2 <- plotly::ggplotly(p2, tooltip = "text")
      
      # Helper functions for data frames
      create_variable_dfs <- function(scaled_dfs, name_columns) {
        variable_dfs <- lapply(name_columns, function(variable_name) {
          summary_data <- scaled_dfs[[variable_name]] %>%
            dplyr::select(time, mean, median, mode)
          return(summary_data)
        })
        names(variable_dfs) <- name_columns
        return(variable_dfs)
      }
      
      create_state_df <- function(simulation_results) {
        state_df <- simulation_results %>%
          dplyr::select(time, mean, median, mode)
        return(state_df)
      }
      
      return(list(
        centroids_plot = all_plots, 
        states_plots = p2, 
        variable_dfs = create_variable_dfs(scaled_dfs, name_columns), 
        state_df = create_state_df(simulation_results)
      ))
    }
  }
  
  if(!is.numeric(state_time_data[[time_column]])){
    if (forecast_type == "ex-post") {
      
      test_dataset <- actual_data
      test_data <- state_time_data[state_time_data[[time_column]] %in% simulation_results$time, ]
      colnames(test_data)[colnames(test_data) == time_column] <- 'time'
      
      actual_raw_dfs <- lapply(name_columns, function(col_name) {
        test_dataset %>%
          dplyr::select(all_of(time_column), all_of(col_name)) %>%
          dplyr::rename(time = !!time_column, !!paste0("actual_", col_name) := all_of(col_name))
      })
      names(actual_raw_dfs) <- name_columns
      
      # Generate all plots for actual vs predicted and residuals
      all_plots <- lapply(name_columns, function(variable_name) {
        
        plot_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, mean, median, mode)
        
        predicted_metric <- summary_data[[mae_metric]]
        actual_col_name <- paste0("actual_", variable_name)
        
        residuals_df <- data.frame(
          t = test_dataset[[time_column]][seq_along(predicted_metric)],
          x = actual_raw_dfs[[variable_name]][[actual_col_name]][seq_along(predicted_metric)],
          predicted_metric
        )
        residuals_df$residuals <- residuals_df$x - residuals_df$predicted_metric
        residuals_sd <- stats::sd(residuals_df$residuals)
        residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
        residuals_df$mape_component <- abs((residuals_df$x - residuals_df$predicted_metric) / residuals_df$x) * 100
        options(scipen = 999)
        # mape <- round(mean(residuals_df$mape_component), 4)  # Commented out as not used
        mae <- round(mean(abs(residuals_df$residuals)), 4)
        
        # Create plots
        p1 <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, variable_name)
        p1 <- add_actual_layer(p1, actual_raw_dfs[[variable_name]], variable_name)
        p1 <- apply_time_scaling(p1, summary_data)
        
        p2 <- create_residual_plot(residuals_df, variable_name)
        p2 <- apply_time_scaling(p2, summary_data)
        
        # Convert to plotly
        p1_plotly <- apply_plotly_layout(p1, type, variable_name, 400)
        p2_plotly <- apply_plotly_layout(p2, type, NULL, 250)
        
        # Create combined layout
        x_plots <- create_html_layout(p1_plotly, p2_plotly, type, variable_name, mae_metric, mae)
        
        list(centroids_plot = x_plots, mae = mae)
      })
      
      # States plot
      plot_data <- simulation_results %>%
        dplyr::select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
      
      summary_data <- simulation_results %>%
        dplyr::select(time, mean, median, mode)
      
      # Analyze timestamps
      time_analysis <- analyze_timestamps(summary_data, "time")
      summary_data <- time_analysis$standardized_data
      if(!is.null(plot_data)) {
        plot_data <- standardize_timestamp(plot_data, "time")
      }
      
      pa <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, NULL, TRUE) +
        ggplot2::geom_line(data = test_data, ggplot2::aes(x = time, y = Cell.ID, color = "Actual States"), size = 1.0) +
        ggplot2::geom_point(data = test_data, ggplot2::aes(x = time, y = Cell.ID, color = "Actual States", 
                                                          text = paste("Time:", time, "<br>Actual States:", Cell.ID)), 
                           size = 1.5, show.legend = TRUE) +
        ggplot2::scale_colour_manual(values = c("Simulations" = "darkgray", "Median" = "red",
                                               "Mean" = "darkgreen", "Actual States" = "black", "Mode" = "#0901FF")) +
        ggplot2::scale_y_continuous(limits = c(1, NA), breaks = integer_breaks()) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        theme_plot
      
      pa <- apply_time_scaling(pa, summary_data)
      
      # States residual plot
      predicted_metric <- summary_data[[mae_metric]]
      residuals_df <- data.frame(test_data[seq_along(predicted_metric),], predicted_metric)
      residuals_df$residuals <- (residuals_df$Cell.ID - residuals_df$predicted_metric)
      residuals_sd <- stats::sd(residuals_df$residuals)
      residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
      residuals_df$mape_component <- abs((residuals_df$Cell.ID - residuals_df$predicted_metric) / residuals_df$Cell.ID) * 100
      options(scipen = 999)
      mape <- round(mean(residuals_df$mape_component), 4)
      mae <- round(mean(abs(residuals_df$residuals)), 4)
      
      pb <- create_residual_plot(residuals_df, NULL)
      pb <- apply_time_scaling(pb, summary_data)
      
      # Convert to plotly
      pa_plotly <- apply_plotly_layout(pa, type, NULL, 400)
      pb_plotly <- apply_plotly_layout(pb, type, NULL, 250)
      
      # Create states layout
      x_plots <- create_html_layout(pa_plotly, pb_plotly, type, "States", mae_metric, mae)
      states_plots <- list(x_plots, mae = mae)
      
      return(list(all_plots, states_plots))
      
    } else {
      # Ex-ante forecasts for non-numeric time
      all_plots <- lapply(name_columns, function(variable_name) {
        plot_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          dplyr::select(time, mean, median, mode)
        
        # Analyze timestamps
        time_analysis <- analyze_timestamps(summary_data, "time")
        summary_data <- time_analysis$standardized_data
        if(!is.null(plot_data)) {
          plot_data <- standardize_timestamp(plot_data, "time")
        }
        
        p1 <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, variable_name)
        p1 <- apply_time_scaling(p1, summary_data)
        p1 <- p1 + ggplot2::labs(x = "Timestamps", y = paste0(variable_name, " (raw units)"),
                                 title = paste0(type, ": Ex-Ante Predicted ", variable_name), color = " ")
        
        plotly::ggplotly(p1, tooltip = "text")
      })
      
      # States plot for ex-ante
      plot_data <- simulation_results %>%
        dplyr::select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(cols = starts_with("Sim_"), names_to = "simulation", values_to = "value")
      summary_data <- simulation_results %>%
        dplyr::select(time, mean, median, mode)
      
      # Analyze timestamps
      time_analysis <- analyze_timestamps(summary_data, "time")
      summary_data <- time_analysis$standardized_data
      if(!is.null(plot_data)) {
        plot_data <- standardize_timestamp(plot_data, "time")
      }
      
      p2 <- create_base_plot(plot_data, summary_data, show_simulation, mae_metric, NULL, TRUE) +
        ggplot2::scale_y_continuous(limits = c(1, NA), breaks = integer_breaks()) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = "Timestamps", y = "States", title = paste0(type, ": Ex-Ante Predicted States"), color = " ") +
        theme_plot
      
      p2 <- apply_time_scaling(p2, summary_data)
      
      p2 <- plotly::ggplotly(p2, tooltip = "text")
      
      # Helper functions for data frames
      create_variable_dfs <- function(scaled_dfs, name_columns) {
        variable_dfs <- lapply(name_columns, function(variable_name) {
          summary_data <- scaled_dfs[[variable_name]] %>%
            dplyr::select(time, mean, median, mode)
          return(summary_data)
        })
        names(variable_dfs) <- name_columns
        return(variable_dfs)
      }
      
      create_state_df <- function(simulation_results) {
        state_df <- simulation_results %>%
          dplyr::select(time, mean, median, mode)
        return(state_df)
      }
      
      return(list(
        centroids_plot = all_plots, 
        states_plots = p2, 
        variable_dfs = create_variable_dfs(scaled_dfs, name_columns), 
        state_df = create_state_df(simulation_results)
      ))
    }
  }
}
