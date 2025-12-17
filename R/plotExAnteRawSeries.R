#' @name plotExAnteRawSeries
#' @title Ex-ante raw series forecasting
#' @description Transforms ex-ante forecasts generated on a log-difference-12 scale back to the original raw data scale
#' @param ex_ante_results List. Output from `msm()` function with `forecast_type = "ex-ante"`
#' @param original_dataset Dataframe. The dataset imported with all features in its original scale of measures 
#' without normalization or log difference
#' @param transformed_dataset Dataframe. The dataset that is transformed for analysis including scaling and log difference
#' @param time_column Character. Name of the time column in the dataset.
#' @param mae_metric Character. Metric to highlight in plots ("mean", "median", or "mode"). Default is "median".
#' @return A list containing:
#' \item{reverted_forecasts}{List of data frames, one per variable, with columns: time, mean, median, mode (reverted values)}
#' \item{plots}{List of plotly objects, one per variable, showing historical data and reverted forecasts}
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords Timeseries_Analysis
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom dplyr select mutate across everything between case_when
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline scale_colour_manual scale_x_datetime 
#' @importFrom ggplot2 scale_x_continuous theme_minimal labs expansion element_text unit
#' @examples 
#' \dontrun{
#' # After running msm() for ex-ante forecasting
#' # Note: trainHVT_results and scoreHVT_results are needed for msm(), 
#' # but NOT for plotExAnteRawSeries() which uses the forecast values directly
#' ex_ante <- msm(state_time_data = temporal_data,
#'                forecast_type = "ex-ante",
#'                transition_probability_matrix = prob_trans_matx,
#'                initial_state = tail(temporal_data$Cell.ID, 1),
#'                n_ahead_ante = ex_ante_period,
#'                num_simulations = 500,
#'                scoreHVT_results = scoring,
#'                trainHVT_results = hvt.results,
#'                raw_dataset = entire_dataset,
#'                time_column = "t")
#' 
#' # Revert to raw scale - only needs ex_ante_results and original_dataset
#' raw_forecasts <- plotExAnteRawSeries(
#'   ex_ante_results = ex_ante,
#'   original_dataset = entire_dataset_original,  # Pre-transformation raw data
#'   transformed_dataset = entire_dataset,        # Post-transformation data 
#'   time_column = "t",
#'   mae_metric = "median"
#' )
#' 
#' # Access reverted forecasts
#' raw_forecasts$reverted_forecasts$CPI_Food
#' 
#' # Access plots
#' raw_forecasts$plots$CPI_Food
#' }
#' @export plotExAnteRawSeries

plotExAnteRawSeries <- function(ex_ante_results,
                            original_dataset,
                            transformed_dataset = NULL,
                            time_column,
                            mae_metric = "median") {
  
  # Global variables for CRAN warnings
  time <- value <- mean <- median <- mode <- NULL
  
  # ============================================================================
  # VALIDATION
  # ============================================================================
  
  # Check ex_ante_results structure
  if (is.null(ex_ante_results) || !is.list(ex_ante_results)) {
    stop("ERROR: ex_ante_results must be a list output from msm() function")
  }
  
  if (is.null(ex_ante_results$plots) || is.null(ex_ante_results$plots$variable_dfs)) {
    stop("ERROR: ex_ante_results must contain plots$variable_dfs from ex-ante forecasting")
  }
  
  # Check original_dataset
  if (is.null(original_dataset) || !is.data.frame(original_dataset)) {
    stop("ERROR: original_dataset must be a data frame")
  }
  
  if (!time_column %in% colnames(original_dataset)) {
    stop(paste0("ERROR: time_column '", time_column, "' not found in original_dataset"))
  }
  
  # Check mae_metric
  if (!mae_metric %in% c("mean", "median", "mode")) {
    stop("ERROR: mae_metric must be 'mean', 'median', or 'mode'")
  }
  
  # Check if we have enough original data (need at least 12 periods)
  if (nrow(original_dataset) < 12) {
    stop("ERROR: original_dataset must have at least 12 periods for log-diff-12 reversion")
  }
  
  # ============================================================================
  # EXTRACT FORECAST DATA
  # ============================================================================
  
  # Get variable forecasts from ex_ante_results
  # variable_dfs already contains transformed forecast values (mean, median, mode) for each variable
  variable_dfs <- ex_ante_results$plots$variable_dfs
  
  if (is.null(variable_dfs) || length(variable_dfs) == 0) {
    stop("ERROR: No variable forecasts found in ex_ante_results$plots$variable_dfs")
  }
  
  # Get variable names
  variable_names <- names(variable_dfs)
  
  # Check that all variables exist in original_dataset
  missing_vars <- setdiff(variable_names, colnames(original_dataset))
  if (length(missing_vars) > 0) {
    stop(paste0("ERROR: Variables not found in original_dataset: ", paste(missing_vars, collapse = ", ")))
  }
  
  # Validate that variable_dfs contains required columns (time, mean, median, mode)
  for (var_name in variable_names) {
    if (!all(c("time", "mean", "median", "mode") %in% colnames(variable_dfs[[var_name]]))) {
      stop(paste0("ERROR: variable_dfs[[", var_name, "]] must contain columns: time, mean, median, mode"))
    }
  }
  
  # ============================================================================
  # HELPER FUNCTIONS (from mcmc_plots.R)
  # ============================================================================
  
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
      dplyr::between(days_diff, 57, 62) ~ "bimonthly",
      dplyr::between(days_diff, 27, 31) ~ "monthly",
      dplyr::between(days_diff, 88, 92) ~ "quarterly",
      dplyr::between(days_diff, 364, 366) ~ "yearly",
      dplyr::between(days_diff, 6, 8) ~ "weekly",
      dplyr::between(days_diff, 13, 15) ~ "biweekly",
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
  
  # ============================================================================
  # REVERSION ALGORITHM
  # ============================================================================
  
  # Standardize original dataset time column
  original_dataset <- standardize_timestamp(original_dataset, time_column)
  
  # Get last 12 original values for each variable
  n_orig <- nrow(original_dataset)
  last_12_indices <- (n_orig - 11):n_orig
  
  # Initialize reverted forecasts list
  reverted_forecasts <- list()
  
  # Process each variable
  for (var_name in variable_names) {
    
    # Get forecast data from variable_dfs (already contains transformed values)
    forecast_data <- variable_dfs[[var_name]]
    forecast_times <- forecast_data$time
    
    # Get transformed forecast values directly from variable_dfs (log-diff-12 space)
    forecast_mean <- as.numeric(forecast_data$mean)
    forecast_median <- as.numeric(forecast_data$median)
    forecast_mode <- as.numeric(forecast_data$mode)
    
    n_forecast <- length(forecast_mean)
    
    # Check for missing values
    if (any(is.na(forecast_mean)) || any(is.na(forecast_median)) || any(is.na(forecast_mode))) {
      warning(paste("Some forecasted values are NA for variable:", var_name))
    }
    
    # Get last 12 original values for this variable
    original_values <- original_dataset[[var_name]][last_12_indices]
    
    # Check for non-positive values
    if (any(original_values <= 0, na.rm = TRUE)) {
      stop(paste0("ERROR: Variable '", var_name, "' has non-positive values in last 12 periods. Log-diff-12 requires positive values."))
    }
    
    # Initialize reverted vectors
    reverted_mean <- numeric(n_forecast)
    reverted_median <- numeric(n_forecast)
    reverted_mode <- numeric(n_forecast)
    
    # Apply reversion formula
    for (i in 1:n_forecast) {
      if (i <= 12) {
        # For first 12 periods: use original value from 12 periods before
        base_index <- n_orig - 12 + i
        base_value <- round(original_dataset[[var_name]][base_index], 2)  # Round raw value to 2 decimals
        
        # Reversion: X_reverted = X_original × exp(Z_forecast)
        # Round exponential values to 4 decimals, then round final result to 2 decimals
        exp_mean <- round(exp(forecast_mean[i]), 4)
        exp_median <- round(exp(forecast_median[i]), 4)
        exp_mode <- round(exp(forecast_mode[i]), 4)
        
        reverted_mean[i] <- round(base_value * exp_mean, 2)
        reverted_median[i] <- round(base_value * exp_median, 2)
        reverted_mode[i] <- round(base_value * exp_mode, 2)
      } else {
        # For periods > 12: use previously reverted value from 12 periods back
        # Round exponential values to 4 decimals, then round final result to 2 decimals
        exp_mean <- round(exp(forecast_mean[i]), 4)
        exp_median <- round(exp(forecast_median[i]), 4)
        exp_mode <- round(exp(forecast_mode[i]), 4)
        
        reverted_mean[i] <- round(reverted_mean[i - 12] * exp_mean, 2)
        reverted_median[i] <- round(reverted_median[i - 12] * exp_median, 2)
        reverted_mode[i] <- round(reverted_mode[i - 12] * exp_mode, 2)
      }
    }
    
    # Create reverted forecast data frame
    reverted_forecasts[[var_name]] <- data.frame(
      time = forecast_times,
      mean = reverted_mean,    # Already rounded to 2 decimals
      median = reverted_median, # Already rounded to 2 decimals
      mode = reverted_mode,     # Already rounded to 2 decimals
      stringsAsFactors = FALSE
    )
  }
  
  # ============================================================================
  # PLOT GENERATION
  # ============================================================================
  
  # Define theme_plot
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
  
  # Initialize plots list
  plots <- list()
  
  # Generate plot for each variable
  for (var_name in variable_names) {
    
    variable_data <- reverted_forecasts[[var_name]]
    
    if (is.null(variable_data) || nrow(variable_data) == 0) {
      warning(paste("Skipping plot for variable:", var_name, "- no data"))
      next
    }
    
    # Standardize timestamps
    time_analysis <- analyze_timestamps(variable_data, "time")
    variable_data <- time_analysis$standardized_data
    
    # Get historical data (last 12 points)
    historical_data <- NULL
    if (var_name %in% colnames(original_dataset) && n_orig >= 12) {
      time_points_last12 <- original_dataset[[time_column]][last_12_indices]
      last_values <- original_dataset[[var_name]][last_12_indices]
      
      if (length(time_points_last12) == length(last_values) && length(last_values) > 0) {
        historical_data <- data.frame(
          time = time_points_last12,
          value = last_values,
          stringsAsFactors = FALSE
        )
        # Standardize historical timestamps
        historical_data <- standardize_timestamp(historical_data, "time")
      }
    }
    
    # Get the 13th point (first point after initial forecast)
    dyn_forecast_time <- NULL
    if (nrow(variable_data) >= 13) {
      dyn_forecast_time <- variable_data$time[13]
    }
    
    # Create base plot
    p1 <- ggplot2::ggplot()
    
    # Add vertical lines
    vline_data <- data.frame(
      time = as.numeric(variable_data$time[1]), 
      label = "Initial Point Forecast"
    )
    
    if (!is.null(dyn_forecast_time)) {
      vline_data_dyn <- data.frame(
        time = as.numeric(dyn_forecast_time),
        label = "Initial Point Dynamic Forecast"
      )
    }
    
    if (!is.null(historical_data)) {
      p1 <- p1 + 
        ggplot2::geom_vline(data = vline_data, 
                           ggplot2::aes(xintercept = time, color = label), 
                           linetype = "dashed", size = 0.5, show.legend = TRUE)
    }
    
    if (!is.null(dyn_forecast_time)) {
      p1 <- p1 + 
        ggplot2::geom_vline(data = vline_data_dyn, 
                           ggplot2::aes(xintercept = time, color = label), 
                           linetype = "dashed", size = 0.5, show.legend = TRUE)
    }
    
    # Add historical data
    if (!is.null(historical_data)) {
      p1 <- p1 +
        ggplot2::geom_line(data = historical_data, 
                          ggplot2::aes(x = time, y = value, color = "Historical"), 
                          size = 1.0) +
        ggplot2::geom_point(data = historical_data, 
                           ggplot2::aes(x = time, y = value, color = "Historical",
                                       text = paste("Time:", time, "<br>Historical", var_name, ":", round(value, 2))), 
                           size = 1.5)
    }
    
    # Add forecast lines and points
    p1 <- p1 +
      ggplot2::geom_line(data = variable_data, 
                        ggplot2::aes(x = time, y = mode, color = "Mode"), 
                        size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
      ggplot2::geom_point(data = variable_data, 
                          ggplot2::aes(x = time, y = mode, color = "Mode", 
                                      text = paste("Time:", time, "<br>Mode", var_name, ":", round(mode, 2))), 
                          size = ifelse(mae_metric == "mode", 1.5, 1)) +
      
      ggplot2::geom_line(data = variable_data, 
                        ggplot2::aes(x = time, y = mean, color = "Mean"), 
                        size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
      ggplot2::geom_point(data = variable_data, 
                         ggplot2::aes(x = time, y = mean, color = "Mean", 
                                     text = paste("Time:", time, "<br>Mean", var_name, ":", round(mean, 2))), 
                         size = ifelse(mae_metric == "mean", 1.5, 1)) +
      
      ggplot2::geom_line(data = variable_data, 
                        ggplot2::aes(x = time, y = median, color = "Median"), 
                        size = ifelse(mae_metric == "median", 1.0, 0.5)) +
      ggplot2::geom_point(data = variable_data, 
                         ggplot2::aes(x = time, y = median, color = "Median", 
                                     text = paste("Time:", time, "<br>Median", var_name, ":", round(median, 2))), 
                         size = ifelse(mae_metric == "median", 1.5, 1)) +
      
      ggplot2::scale_colour_manual(values = c("Median" = "red", 
                                              "Mean" = "darkgreen", 
                                              "Mode" = "#0901FF",
                                              "Historical" = "black",
                                              "Initial Point Forecast" = "gray",
                                              "Initial Point Dynamic Forecast" = "purple"
      ))
    
    # Combine all timestamps for axis breaks
    all_times <- variable_data$time
    if (!is.null(historical_data)) {
      all_times <- c(historical_data$time, all_times)
    }
    
    # Add appropriate axis scaling
    if (inherits(variable_data$time, "POSIXct")) {
      p1 <- p1 + 
        ggplot2::scale_x_datetime(
          breaks = all_times,
          date_labels = "%Y-%m",
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else if (is.numeric(variable_data$time)) {
      p1 <- p1 + 
        ggplot2::scale_x_continuous(
          breaks = all_times,
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
    
    # Add labels and theme
    p1 <- p1 +
      ggplot2::labs(
        x = "Timestamps",
        y = paste(var_name, "(raw units)"),
        title = paste("MSM: Ex-ante Predicted", var_name),
        color = " "
      ) +
      ggplot2::theme_minimal() +
      theme_plot
    
    # Convert to plotly
    p1_plotly <- plotly::ggplotly(p1, tooltip = "text") %>%
      plotly::layout(
        margin = list(r = 150, b = 50, t = 50),
        height = 400,
        yaxis = list(
          title = list(
            text = paste(var_name, "(raw units)"),
            standoff = 10
          )
        ),
        xaxis = list(
          title = list(
            text = "Timestamps",
            standoff = 10
          ),
          tickangle = -45
        ),
        legend = list(
          title = list(text = "")
        )
      )
    
    plots[[var_name]] <- p1_plotly
  }
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  result <- list(
    reverted_forecasts = reverted_forecasts,
    plots = plots
  )
  
  class(result) <- "hvt.object"
  return(result)
}
