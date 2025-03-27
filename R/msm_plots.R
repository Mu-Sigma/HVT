utils::globalVariables(c("variable_name"))

msm_plots <- function(simulation_results, centroid_data,centroid_2d_points, actual_data, 
                        state_time_data ,forecast_type,
                       n_ahead_ante, type,raw_dataset, show_simulation,
                       mae_metric=mae_metric, time_column,trainHVT_results,plot_type) {
  
  requireNamespace("patchwork")
  ##for cran warnings
  time <- simulation <- median <- sd<- studentized_residuals <-NULL
  ######### extracting the simulation results without statistics ########
  simulation_results_wo_statistics <- simulation_results %>%
    select(-mean, -median, -mode)
  
  ######### recording the column name ###########
  name_columns <- colnames(centroid_data)
  centroid_data <- centroid_data %>% mutate(across(where(is.numeric), ~ round(., 4)))
  
  ####### calculating the mean and sd of raw dataset ########
  raw_dataset_wo_time <- raw_dataset %>% dplyr::select(-(time_column))
  mean_raw <- raw_dataset_wo_time %>% summarise(across(everything(), ~ round(mean(.), 4)))
  sd_raw <- raw_dataset_wo_time %>% summarise(across(everything(), ~ round(stats::sd(.), 4)))

  #####################################

  
  ######### joining the dataframe ##########
  centroid_dataframe <- cbind(centroid_data, Cell.ID = centroid_2d_points$Cell.ID)
  
  ########### Function to generate the predicted dataframe based on coordinate type #########
  generate_predicted_df <- function(coord) {
    centroid_map <- centroid_dataframe %>%
      select(Cell.ID, coord)
    
    replace_with_value <- function(column) {
      centroid_map[[coord]][match(column, centroid_map$Cell.ID)]
    }
    
   
    sim_df <- simulation_results[,-1] %>%
      mutate_all(replace_with_value) %>%
      rename_with(~ paste0(., "_", coord), starts_with("Sim_"))
    

    simulation_results %>%
      select(time) %>%
      bind_cols(sim_df)
  }
  
  predicted_dfs <- lapply(name_columns, generate_predicted_df)
  names(predicted_dfs) <- name_columns
  
  ########### Scaling the predicted centroids ##########
  
  if(trainHVT_results[["model_info"]][["input_parameters"]][["normalize"]]){
  scaled_dfs <- lapply(names(predicted_dfs), function(name) {
    scale_df <- predicted_dfs[[name]] %>%
      dplyr::select(-time) %>%
      mutate(across(everything(), ~ (. *  sd_raw[1, name]) + mean_raw[1, name])) 
    
  
    scale_df <- round(scale_df,4)
    predicted_df <- cbind(time = predicted_dfs[[name]]$time, scale_df)
    return(predicted_df)
  })
  } else{
    scaled_dfs <- predicted_dfs
  }
  
  names(scaled_dfs) <- names(predicted_dfs)
 
################################################################################################################################### 
  standardize_timestamp <- function(data, time_col) {
    # Get first non-NA value
    sample_time <- stats::na.omit(data[[time_col]])[1]
    
    # If already a POSIXct, return as is
    if(inherits(sample_time, "POSIXct")) {
      return(data)
    }
    
    # List of allowed formats to try
    formats <- c(
      "%m/%d/%Y",
      "%m-%d-%Y",
      "%Y/%m/%d",
      "%Y-%m-%d",
      "%Y-%m-%d %H:%M:%S",
      "%d/%m/%Y"
    )
    
    # Try each format
    for(fmt in formats) {
      tryCatch({
        data[[time_col]] <- as.POSIXct(data[[time_col]], format = fmt)
        if(!all(is.na(data[[time_col]]))) {
          break
        }
      }, error = function(e) {})
    }
    
    return(data)
  }
  
  # Function to analyze timestamp characteristics
  analyze_timestamps <- function(data, time_col) {
    # Standardize the timestamps
    data <- standardize_timestamp(data, time_col)
    
    # Sort timestamps to analyze progression
    sorted_times <- sort(data[[time_col]])
    
    # Check what changes between consecutive timestamps
    changes <- list(
      year = length(unique(format(sorted_times, "%Y"))),
      month = length(unique(format(sorted_times, "%Y-%m"))),
      day = length(unique(format(sorted_times, "%Y-%m-%d"))),
      hour = length(unique(format(sorted_times, "%Y-%m-%d %H"))),
      minute = length(unique(format(sorted_times, "%Y-%m-%d %H:%M"))),
      second = length(unique(format(sorted_times, "%Y-%m-%d %H:%M:%S")))
    )
    
    # Determine smallest changing unit and set format
    date_format <- if(changes$minute == changes$second) {
      if(changes$hour == changes$minute) {
        if(changes$day == changes$hour) {
          if(changes$month == changes$day) {
            if(changes$year == changes$month) {
              "%Y"
            } else {
              "%Y-%m"
            }
          } else {
            "%Y-%m-%d"
          }
        } else {
          "%Y-%m-%d %H"
        }
      } else {
        "%Y-%m-%d %H:%M"
      }
    } else {
      "%Y-%m-%d %H:%M:%S"
    }
    
    # Calculate time differences and interval type
    time_diffs <- diff(sorted_times)
    median_diff <- median(time_diffs)
    days_diff <- as.numeric(median_diff, units="days")
    
    # Determine the predominant interval
    interval_type <- case_when(
      between(days_diff, 57, 62) ~ "bimonthly",  # Add a new interval type for 2-month periods
      between(days_diff, 27, 31) ~ "monthly",
      between(days_diff, 88, 92) ~ "quarterly",
      between(days_diff, 364, 366) ~ "yearly",
      between(days_diff, 6, 8) ~ "weekly",
      between(days_diff, 13, 15) ~ "biweekly",
      days_diff < 1 ~ "intraday",
      days_diff == 1 ~ "daily",
      TRUE ~ "irregular"
    )
    
    # Calculate total time span
    total_span <- as.numeric(difftime(max(sorted_times), min(sorted_times), units = "days"))
    
    return(list(
      interval_type = interval_type,
      date_format = date_format,
      total_span = total_span,
      standardized_data = data
    ))
  }
  
  # Function to get regular timestamp breaks
  get_regular_breaks <- function(data_time, interval_type) {
    # Sort and get first timestamp
    data_time <- sort(data_time)
    first_timestamp <- data_time[1]
    max_timestamp <- max(data_time)
    
    # Custom 2-month interval generation
    if(interval_type == "bimonthly") {
      # Generate breaks exactly 2 months apart
      breaks <- seq(from = first_timestamp, 
                    to = max_timestamp, 
                    by = "2 months")
      
      # Ensure we don't exceed 10 breaks
      if(length(breaks) > 10) {
        step <- ceiling(length(breaks) / 10)
        indices <- c(1, seq(from = 1 + step, to = length(breaks), by = step))
        breaks <- breaks[indices]
      }
      
      return(breaks)
    }
    
    # Existing logic for other interval types
    interval <- switch(interval_type,
                       "daily" = "1 day",
                       "weekly" = "1 week", 
                       "biweekly" = "2 weeks",                            
                       "monthly" = "1 month",
                       "quarterly" = "3 months",
                       "yearly" = "1 year",
                       "intraday" = "6 hours",
                       "irregular" = "1 month"
    )
    
    # Calculate number of breaks
    total_days <- as.numeric(difftime(max_timestamp, first_timestamp, units = "days"))
    
    # Create sequence with first timestamp and regular intervals
    breaks <- seq(from = first_timestamp, 
                  to = max_timestamp, 
                  by = interval)
    
    # If too many breaks, thin them out while keeping first point
    n_breaks <- 20
    if(length(breaks) > n_breaks) {
      step <- ceiling(length(breaks) / n_breaks)
      indices <- c(1, seq(from = 1 + step, to = length(breaks), by = step))
      breaks <- breaks[indices]
    }
    
    return(breaks)
  }
  # Function to determine appropriate axis breaks
  get_axis_breaks <- function(time_analysis) {
    # Map interval_type to break_interval
    break_interval <- switch(time_analysis$interval_type,
                             "intraday" = if(grepl("%H:%M:%S", time_analysis$date_format)) "1 minute" 
                             else if(grepl("%H:%M", time_analysis$date_format)) "15 minutes" 
                             else "1 hour",
                             "daily" = "1 day",
                             "weekly" = "1 week",
                             "biweekly" = "2 weeks",
                             "monthly" = "1 month",
                             "quarterly" = "3 months",
                             "yearly" = "1 year",
                             "bimonthly" = "2 months",
                             
                             {
                               # For irregular data, base on total span
                               months_span <- time_analysis$total_span / 30.44
                               if(months_span <= 1) "1 week"
                               else if(months_span <= 3) "2 weeks"
                               else if(months_span <= 6) "1 month"
                               else if(months_span <= 24) "1 month"
                               else if(months_span <= 60) "2 months"
                               else "1 year"
                             }
    )
    
    return(list(
      break_interval = break_interval,
      date_format = time_analysis$date_format
    ))
  } 

  theme_plot <-  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    legend.key.width = unit(0.5, "cm")
  )
  
  integer_breaks <- function(n = 5, ...) {
    function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
  }
  
#################################################################################################################################333

  if(is.numeric(state_time_data$t)){  
    ########### Ex-Post Actual vs Predicted Plot ##########
    if (forecast_type == "ex-post") {
      
      
      test_dataset <- actual_data
      ####################
      
     # test_data <- tail(state_time_data, nrow(test_dataset))
      test_data <- state_time_data[state_time_data$t %in% simulation_results$time, ]
      
      ##########################
      
      actual_raw_dfs <- lapply(name_columns, function(col_name) {
        df <- test_dataset[, c("t", col_name)]
        names(df) <- c("time", paste0("actual_", col_name))
        return(df)
      })
      names(actual_raw_dfs) <- name_columns
      # Generate all plots for actual vs predicted and residuals
      all_plots <- lapply(name_columns, function(variable_name) {
        
        # Data manipulation for plotting
        plot_data <- scaled_dfs[[variable_name]] %>%
          select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(
            cols = starts_with("Sim_"),
            names_to = "simulation",
            values_to = "value"
          )
        
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          select(time, mean, median, mode)
        
        predicted_metric <- summary_data[[mae_metric]]
        
        actual_col_name <- paste0("actual_", variable_name)
        
        residuals_df <- data.frame(
          t = test_dataset$t[1:length(predicted_metric)],
          x = actual_raw_dfs[[variable_name]][[actual_col_name]][1:length(predicted_metric)],
          predicted_metric
        )
        residuals_df$residuals <- residuals_df$x - residuals_df$predicted_metric
        residuals_sd <- sd(residuals_df$residuals)
        residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
        residuals_df$mape_component <- abs((residuals_df$x - residuals_df$predicted_metric) / residuals_df$x) * 100
        options(scipen = 999)
        mape <- round(mean(residuals_df$mape_component),4)
        mae <- round(mean(abs(residuals_df$residuals)), 4)
        
#browser()      
        
        # Actual vs Predicted Plot (p1)
        p1 <- ggplot() +
          {if(show_simulation) geom_line(data = plot_data,
                                         aes(x = time, y = value, group = simulation, 
                                             colour = "Simulations",
                                             text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), 
                                         alpha = 0.4, size = 0.4)} +
          geom_line(data = summary_data, aes(x = time, y = mode, 
                                             colour = "Mode"), 
                    size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = mode, 
                                              colour = "Mode",
                                              text = paste("Time:", time, "<br>Mode", variable_name, " :", mode)), 
                     size = ifelse(mae_metric == "mode", 1.5, 1.0)) +
          
          geom_line(data = summary_data, aes(x = time, y = mean, 
                                             colour = "Mean"), 
                    size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = mean, 
                                              colour = "Mean",
                                              text = paste("Time:", time, "<br>Mean", variable_name, " :", mean)), 
                     size = ifelse(mae_metric == "mean", 1.5, 1.0)) +
          geom_line(data = summary_data, aes(x = time, y = median, 
                                             colour = "Median"), 
                    size = ifelse(mae_metric == "median", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = median, 
                                              colour = "Median",
                                              text = paste("Time:", time, "<br>Median", variable_name, " :", median)), 
                     size = ifelse(mae_metric == "median", 1.5, 1.0)) +
          geom_line(data = actual_raw_dfs[[variable_name]], 
                    aes(x = time, y = !!sym(actual_col_name), 
                        colour = "Actual"), 
                    size = 1.0) +
          geom_point(data = actual_raw_dfs[[variable_name]], 
                     aes(x = time, y = !!sym(actual_col_name), 
                         colour = "Actual",
                         text = paste("Time:", time, "<br>Actual", variable_name, " :", round(!!sym(actual_col_name),4))),
                     size = 1.5) +
          scale_colour_manual(values = c("Simulations" = "darkgray", 
                                         "Median" = "red", 
                                         "Mean" = "darkgreen", 
                                         "Actual" = "black", 
                                         "Mode" = "#0901FF")) +
          theme_minimal() +
          labs(x = "Timestamps", 
               y = paste0(variable_name, " (raw units)"),
               title = paste0(type, ": Ex-Post Actual ", variable_name, " vs Predicted ", variable_name),
               color = " ") + theme_plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
        if(all(residuals_df$residuals == 0)) {
          residuals_df$studentized_residuals <- 0
        }
     
        # Residuals Plot (p2)
        p2 <- ggplot() +
          geom_line(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals"), size = 0.8) +
          geom_point(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals",
                                              text = paste("Time:", t, "<br>Residuals",variable_name, " :", round(studentized_residuals,4))), 
                     size = 1.0) +          
          geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
          geom_hline(yintercept = -1, col = "blue", linetype = "dashed") +
          geom_hline(yintercept = 1, col = "blue", linetype = "dashed") +
          scale_color_manual(name = NULL, labels = function(x) gsub("\n", "\n", x), values = "black") +
          labs(title = paste0(type, ": Ex-Post Studentized Residuals for the ", mae_metric," forecast" ),
               subtitle = paste("MAE:", mae), 
               x = "Timestamps", y = "Studentized Residuals") +
          theme_minimal() +
          theme_plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        
        
        if(plot_type == "static"){
           x_plots <- (p1 / p2) + 
             patchwork::plot_layout(heights = c(2, 1))
        }else{

        p1_plotly <- plotly::ggplotly(p1, tooltip = "text") %>%
          plotly::layout(
            margin = list(r = 150, b = 50, t = 50),
            height = 400,
            yaxis = list(
              title = list(
                text = paste0(variable_name, " (raw units)"),
                            standoff = 10
              )
            ),
            xaxis = list(
              title = list(
                text = "Timestamps",
              standoff = 10
              )
            ),
            legend = list(
              title = list(text = "") 
            )
          )
        
        p2_plotly <- plotly::ggplotly(p2, tooltip = "text") %>%
          plotly::layout(
            margin = list(r = 150, b = 50, t = 60),  # Keep enough top margin
            height = 250, 
            yaxis = list(
              title = list(
                text = "Studentized Residuals",
                standoff = 10
              )
            ),
            xaxis = list(
              title = list(
                text = "Timestamps",
                standoff = 10
              )
            ),
            legend = list(
              title = list(text = "")  
            ),
            annotations = list(
              list(
                x = 0,  # Align to the left
                y = 1.22,  # Slightly below the title
                text = paste("MAE:", mae),  
                showarrow = FALSE,  
                xref = "paper",
                yref = "paper",
                align = "left",  # Align text to the left
                font = list(
                  size = 15.5,  # Two sizes smaller than title
                  family = "Arial",  # Match title font
                  color = "black"
                )
              )
            )
          )
        
        # Create combined plot with HTML layout
        x_plots <- htmltools::tagList(
          htmltools::div(
            style = "display: grid; grid-template-columns: 1fr; max-width: 100%; font-family: Arial, sans-serif;",
           
            htmltools::div(
              style = "grid-column: 1; width: 100%;", 
              p1_plotly
            ),
            
            htmltools::div(
              style = "grid-column: 1; width: 100%;", 
              p2_plotly
            )
          )
        )
        }
        # Return the combined plot
        list(centroids_plot=x_plots, mae = mae)
      })
    
      # Store plots in a list
      plot_list <- (all_plots)
      
      ########### States Plot ###########
      plot_data <- simulation_results %>%
        select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(
          cols = starts_with("Sim_"),
          names_to = "simulation",
          values_to = "value"
        )
      
      summary_data <- simulation_results %>%
        select(time, mean, median, mode)
      
      pa <-  ggplot() +
        {if(show_simulation) geom_line(data = plot_data,aes(x = time, y = value, group = simulation,color = "Simulations", 
                                                            text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), 
                                       alpha = 0.4, size=0.4)} +
        geom_line(data = summary_data,aes(x = time, y = mode,color = "Mode"),size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
        geom_point(data = summary_data,aes(x = time, y = mode,color = "Mode", text = paste("Time:", time, "<br>Mode:", mode)),size = ifelse(mae_metric == "mode", 1.5, 1.0)) +
        
        geom_line(data = summary_data,aes(x = time, y = mean,color = "Mean"), size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
        geom_point(data = summary_data,aes(x = time, y = mean,color = "Mean", text = paste("Time:", time, "<br>Mean:", mean)), size = ifelse(mae_metric == "mean", 1.5, 1.0)) +
        
        geom_line(data = summary_data,aes(x = time, y = median,color = "Median"), size = ifelse(mae_metric == "median", 1.0, 0.4)) +
        geom_point(data = summary_data,aes(x = time, y = median,color = "Median", text = paste("Time:", time, "<br>Median:", median)), size = ifelse(mae_metric == "median", 1.5, 1.0)) +
        
        geom_line(data = test_data,aes(x = t, y = Cell.ID,color = "Actual States"), size = 1) +
        geom_point(data = test_data,aes(x = t, y = Cell.ID,color = "Actual States", text = paste("Time:", t , "<br>Actual States:", Cell.ID)), size = 1.5) +
        
        scale_colour_manual(values = c("Simulations"="darkgray", "Median"="red",
                                       "Mean" =  "darkgreen","Actual States"= "black","Mode"="#0901FF")) +
        scale_y_continuous(
          limits = c(0, NA),  # Ensure y-axis starts from zero
          breaks = integer_breaks()  # Custom function for integer breaks
        ) +
        theme_minimal() +
        labs(x = "Timestamps",
             y = "States",
             title = paste0(type, ": Ex-Post Actual States vs Predicted States"),
             color = " ") +
        theme_plot +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      
      
      
      
      ##############states residual plot###################
      predicted_metric <- summary_data[[mae_metric]]
      residuals_df <- data.frame(test_data[1:length(predicted_metric),], predicted_metric)
      residuals_df$residuals <- ( residuals_df$Cell.ID - residuals_df$predicted_metric)
      residuals_sd <- sd(residuals_df$residuals)
      residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
      residuals_df$mape_component <- abs((residuals_df$Cell.ID - residuals_df$predicted_metric) / residuals_df$Cell.ID) * 100
      options(scipen = 999)
      mape <- round(mean(residuals_df$mape_component),4)
      mae <- round(mean(abs(residuals_df$residuals)), 4)
      
      if(all(residuals_df$residuals == 0)) {
        residuals_df$studentized_residuals <- 0
      }
      
     pb <- ggplot() +
        geom_line(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals"), size = 0.8) +
        geom_point(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals",
                                            text = paste("Time:", t, "<br>Residuals:", round(studentized_residuals,4))), 
                   size = 1.0) +        
        geom_hline(yintercept=0, col="black", linetype="dashed") +
        geom_hline(yintercept=-1, col="blue", linetype="dashed") +
        geom_hline(yintercept=1, col="blue", linetype="dashed") +
        scale_color_manual(name = NULL, labels = function(x) gsub("\n", "\n", x), values = "black") +
        labs(title =paste0(type, ": Ex-Post Studentized Residuals for the ", mae_metric, "forecast"),
             subtitle = paste("MAE:", mae),
             x = "Timestamps", y = "Studentized Residuals") +
        theme_minimal() + theme_plot +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      
     
      if(plot_type == "static"){ 
      ###########plot display output########
      x_plots <- (pa / pb) +
        patchwork::plot_layout(heights = c(2, 1))
      }else{
      pa_plotly <- plotly::ggplotly(pa, tooltip = "text") %>%
        plotly::layout(
          margin = list(r = 150, b = 50, t = 50), 
          height = 400, 
          yaxis = list(
            title = list(
              text = paste0("States"),
              standoff = 10
            )
          ),
          xaxis = list(
            title = list(
              text = "Timestamps",
              standoff = 10
            )
          ),
          legend = list(
            title = list(text = "") 
          )
        )
      
      pb_plotly <- plotly::ggplotly(pb, tooltip = "text") %>%
        plotly::layout(
          margin = list(r = 150, b = 50, t = 60),  # Keep enough top margin
          height = 250, 
          yaxis = list(
            title = list(
              text = "Studentized Residuals",
              standoff = 10
            )
          ),
          xaxis = list(
            title = list(
              text = "Timestamps",
              standoff = 10
            )
          ),
          legend = list(
            title = list(text = "")  
          ),
          annotations = list(
            list(
              x = 0,  # Align to the left
              y = 1.22,  # Slightly below the title
              text = paste("MAE:", mae),  
              showarrow = FALSE,  
              xref = "paper",
              yref = "paper",
              align = "left",  # Align text to the left
              font = list(
                size = 15.5,  # Two sizes smaller than title
                family = "Arial",  # Match title font
                color = "black"
              )
            )
          )
        )
      
      # Create combined plot with HTML layout
      x_plots <- htmltools::tagList(
        htmltools::div(
          style = "display: grid; grid-template-columns: 1fr; max-width: 100%; font-family: Arial, sans-serif;",
       
          
          htmltools::div(
            style = "grid-column: 1; width: 100%;", 
            pa_plotly
          ),
          
          htmltools::div(
            style = "grid-column: 1; width: 100%;", 
            pb_plotly
          )
        )
      )
      }
      states_plots <- list(x_plots, mae =mae)
      
      ########### Return All Plots ###########
      return(list(plot_list, states_plots))
      
      
    }else{
      
      all_plots <- lapply(name_columns, function(variable_name) {
        # Data manipulation for plotting
        plot_data <- scaled_dfs[[variable_name]] %>%
          select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(
            cols = starts_with("Sim_"),
            names_to = "simulation",
            values_to = "value"
          )
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          select(time, mean, median, mode)
        
        
        p1 <- ggplot() +
          {if(show_simulation) geom_line(data = plot_data, 
                                         aes(x = time, y = value, group = simulation, 
                                             color = "Simulations",text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), alpha = 0.4, size = 0.4)} +
          geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), 
                    size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = mode, color = "Mode", text = paste("Time:", time, "<br>Mode",variable_name, " :", mode)), 
                     size = ifelse(mae_metric == "mode", 1.5, 1)) +
          geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), 
                    size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = mean, color = "Mean", text = paste("Time:", time, "<br>Mean",variable_name, " :", mean)), 
                     size = ifelse(mae_metric == "mean", 1.5, 1)) +
          geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), 
                    size = ifelse(mae_metric == "median", 1.0, 0.5)) +
          geom_point(data = summary_data, aes(x = time, y = median, color = "Median", text = paste("Time:", time, "<br>Median",variable_name, " :", median)), 
                     size = ifelse(mae_metric == "median", 1.5, 1)) +
          scale_colour_manual(values = c("Simulations" = "darkgray", 
                                         "Median" = "red", 
                                         "Mean" = "darkgreen", 
                                         "Mode" = "#0901FF")) +
          # scale_y_continuous(breaks = function(x) round(seq(from = floor(x[1]), 
          #                                                   to = ceiling(x[2]), 
          #                                                   length.out = 10))) +
          # {if(inherits(summary_data$time, "POSIXct")) 
          #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
          # } +
          theme_minimal() +
          labs(x = "Timestamps", 
               y = paste0( variable_name, " (raw units)"), 
               title = paste0(type, ": Ex-Ante Predicted ", variable_name),
               color = " ")+theme_plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        
       if(plot_type == "static"){
          p1 <- p1
       }else{
          p1 <- plotly::ggplotly(p1,tooltip = "text")
       }
                 
       return(p1)
      })
      
      # Store plots in a list
      plot_list <- (all_plots)
      
      
      # States plot
      plot_data <- simulation_results %>%
        select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(
          cols = starts_with("Sim_"),
          names_to = "simulation",
          values_to = "value"
        )
      summary_data <- simulation_results %>%
        select(time, mean, median, mode)
      
   
      
      p2 <- ggplot() +
        {if(show_simulation) geom_line(data = plot_data, 
                                       aes(x = time, y = value, group = simulation, 
                                           color = "Simulations",text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), alpha = 0.4, size = 0.4)} +
        geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), 
                  size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
        geom_point(data = summary_data, aes(x = time, y = mode, color = "Mode", text = paste("Time:", time, "<br>Mode:", mode)), 
                   size = ifelse(mae_metric == "mode", 1.5, 1.0)) +
        geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), 
                  size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
        geom_point(data = summary_data, aes(x = time, y = mean, color = "Mean", text = paste("Time:", time, "<br>Mean:", mean)), 
                   size = ifelse(mae_metric == "mean", 1.5, 1.0)) +
        geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), 
                  size = ifelse(mae_metric == "median", 1.0, 0.5)) +
        geom_point(data = summary_data, aes(x = time, y = median, color = "Median", text = paste("Time:", time, "<br>Median:", median)), 
                   size = ifelse(mae_metric == "median", 1.5, 1.0)) +
        scale_colour_manual(values = c("Simulations" = "darkgray", 
                                       "Median" = "red", 
                                       "Mean" = "darkgreen", 
                                       "Mode" = "#0901FF")) +
        scale_y_continuous(
          limits = c(0, NA),  # Ensure y-axis starts from zero
          breaks = integer_breaks()  # Custom function for integer breaks
        ) +
        # {if(inherits(summary_data$time, "POSIXct")) 
        #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
        # } +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      
        theme_minimal() +
        labs(x = "Timestamps",
             y = "States",
             title = paste0(type, ": Ex-Ante Predicted States"),
             color = " ") +
        theme(legend.position = "right")+ theme_plot

    
      if(plot_type == "static"){
        p2 <- p2
      }else{
        p2 <- plotly::ggplotly(p2,tooltip = "text")
      }
      
      return(list(centroids_plot = plot_list, states_plots = p2))
    }
    
  }

  
  if(!is.numeric(state_time_data$t)){
    
    ########### Ex-Post Actual vs Predicted Plot ##########
    if (forecast_type == "ex-post") {
      
      test_dataset <- actual_data
      ####################
      
     # test_data <- tail(state_time_data, nrow(test_dataset))
      test_data <- state_time_data[state_time_data$t %in% simulation_results$time, ]
      
      ##########################
      
      actual_raw_dfs <- lapply(name_columns, function(col_name) {
        test_dataset %>%
          dplyr::select(t, all_of(col_name)) %>%
          rename(time = t, !!paste0("actual_", col_name) := all_of(col_name))
      })
      names(actual_raw_dfs) <- name_columns
      
      # Generate all plots for actual vs predicted and residuals
      all_plots <- lapply(name_columns, function(variable_name) {
        
        # Data manipulation for plotting
        plot_data <- scaled_dfs[[variable_name]] %>%
          select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(
            cols = starts_with("Sim_"),
            names_to = "simulation",
            values_to = "value"
          )
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          select(time, mean, median, mode)
        
        predicted_metric <- summary_data[[mae_metric]]
        actual_col_name <- paste0("actual_", variable_name)
        
        residuals_df <- data.frame(
          t = test_dataset$t[1:length(predicted_metric)],
          x = actual_raw_dfs[[variable_name]][[actual_col_name]][1:length(predicted_metric)],
          predicted_metric
        )
        residuals_df$residuals <- residuals_df$x - residuals_df$predicted_metric
        residuals_sd <- sd(residuals_df$residuals)
        residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
        residuals_df$mape_component <- abs((residuals_df$x - residuals_df$predicted_metric) / residuals_df$x) * 100
        options(scipen = 999)
        mape <- round(mean(residuals_df$mape_component),4)
        mae <- round(mean(abs(residuals_df$residuals)), 4)
        
        # Create plot
        # Analyze timestamps
        time_analysis <- analyze_timestamps(summary_data, "time")
        axis_settings <- get_axis_breaks(time_analysis)
        
        # Use standardized data
        summary_data <- time_analysis$standardized_data
        if(!is.null(plot_data)) {
          plot_data <- standardize_timestamp(plot_data, "time")
        }
        
       p1 <- ggplot() +
          {if(show_simulation)
          geom_line(data = plot_data,aes(x = time, y = value, group = simulation, color = "Simulations",
                                             text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)),
                                             alpha = 0.4, size = 0.4)} +
            geom_line(data = summary_data, aes(x = time, y = mode,
                                              color = "Mode"),
                      size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
            geom_point(data = summary_data, aes(x = time, y = mode,
                                                color = "Mode",
                                                text = paste("Time:", time, "<br>Mode", variable_name, " :", mode)),
                       size = ifelse(mae_metric == "mode", 1.5, 0.8)) +
        
            geom_line(data = summary_data, aes(x = time, y = mean,
                                              color = "Mean"),
                      size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
            geom_point(data = summary_data, aes(x = time, y = mean,
                                                color = "Mean",
                                                text = paste("Time:", time, "<br>Mean", variable_name, " :", mean)),
                       size = ifelse(mae_metric == "mean", 1.5, 0.8)) +
            geom_line(data = summary_data, aes(x = time, y = median,
                                              color = "Median"),
                      size = ifelse(mae_metric == "median", 1.0, 0.5)) +
            geom_point(data = summary_data, aes(x = time, y = median,
                                                color = "Median",
                                                text = paste("Time:", time, "<br>Median", variable_name, " :", median)),
                       size = ifelse(mae_metric == "median", 1.5, 1)) +
         geom_line(data = actual_raw_dfs[[variable_name]],
                   aes(x = time, y = !!sym(actual_col_name),
                       color = "Actual"),
                   size = 1.0) +
         geom_point(data = actual_raw_dfs[[variable_name]],
                    aes(x = time, y = !!sym(actual_col_name),
                        color = "Actual",
                        text = paste("Time:", time, "<br>Actual", variable_name, " :", round(!!sym(actual_col_name),4))),
                    size = 1.5) +
          scale_colour_manual(values = c("Simulations" = "darkgray",
                                         "Median" = "red",
                                         "Mean" = "darkgreen",
                                         "Actual" = "black",
                                         "Mode" = "#0901FF")) +
          theme_minimal() +
          labs(x = "Timestamps",
               y = paste0( variable_name, " (raw units)"),
               title = paste0(type, ": Ex-Post Actual ", variable_name, " vs Predicted ", variable_name),
               color = " ") + theme_plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
       
        
        
        
        # Add x-axis scaling and minor grid separately
        if(inherits(summary_data$time, "POSIXct")) {
          time_analysis <- analyze_timestamps(summary_data, "time")
          axis_settings <- get_axis_breaks(time_analysis)
          all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
          
          p1 <- p1 + 
            scale_x_datetime(
              breaks = all_breaks,
              minor_breaks = summary_data$time, 
              date_labels = axis_settings$date_format,
              expand = expansion(mult = 0.02)
            ) 
        } else {
          p1 <- p1 + 
            scale_x_continuous(
              expand = expansion(mult = 0.02)
            )
        }
        
       if(all(residuals_df$residuals == 0)) {
         residuals_df$studentized_residuals <- 0
       }
        
        p2 <- ggplot() +
          geom_line(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals"), size = 0.8) +
          geom_point(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals",
                                              text = paste("Time:", t, "<br>Residuals",variable_name, " :", round(studentized_residuals,4))), 
                     size = 1.5) +
          geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
          geom_hline(yintercept = -1, col = "blue", linetype = "dashed") +
          geom_hline(yintercept = 1, col = "blue", linetype = "dashed") +
          scale_color_manual(name = NULL, values = c("Studentized\nResiduals" = "black")) +
          labs(title = paste0(type, ": Ex-Post Studentized Residuals for the ", mae_metric," forecast"),
               subtitle = paste("MAE:", mae), 
               x = "Timestamps", y = "Studentized Residuals") +theme_minimal() +
          {
            if(inherits(summary_data$time, "POSIXct")) {
              time_analysis <- analyze_timestamps(summary_data, "time")
              axis_settings <- get_axis_breaks(time_analysis)
              all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
              
              scale_x_datetime(
                breaks = all_breaks,
                minor_breaks = residuals_df$t,
                date_labels = axis_settings$date_format,
                expand = expansion(mult = 0.02)
              )
            } else {
              # Fallback for non-POSIXct data
              scale_x_continuous(
                expand = expansion(mult = 0.02)
              )
            }
          } +
          theme_plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
     
        if(plot_type == "static"){
          # Combine the plots
          x_plots <- (p1 / p2) + 
            patchwork::plot_layout(heights = c(2, 1))
        }else{
        # p1_plotly <- plotly::ggplotly(p1) %>%
        #   layout(
        #     margin = list(r = 150, b = 0, t = 50), 
        #     height = 380, 
        #     yaxis = list(
        #       title = list(
        #         text = paste0(variable_name, " (raw units)"),
        #         standoff = 10
        #       )
        #     ),
        #     xaxis = list(
        #       title = list(
        #         text = "Timestamps",
        #         standoff = 10
        #       )
        #     ),
        #     legend = list(
        #       title = list(text = "") 
        #     )
        #   )
          p1_plotly <- plotly::ggplotly(p1, tooltip = "text") %>%
            plotly::layout(
              margin = list(r = 150, b = 0, t = 50), 
              height = 380, 
              yaxis = list(
                title = list(
                  text = paste0(variable_name, " (raw units)"),
                  standoff = 10
                )
              ),
              xaxis = list(
                title = list(
                  text = "Timestamps",
                  standoff = 10
                )
              ),
              legend = list(
                title = list(text = "")
              ),
              # Add these two lines to improve hover behavior
              hovermode = "closest",
              hoverdistance = 15
            ) %>%
            # Add this config call to enable the toggleHover button
            plotly::config(
              modeBarButtonsToAdd = list("toggleHover"),
              displaylogo = FALSE
            )
          
        
 
        p2_plotly <- plotly::ggplotly(p2, tooltip = "text") %>%
          plotly::layout(
            margin = list(r = 150, b = 0, t = 60),  # Keep enough top margin
            height = 230, 
            yaxis = list(
              title = list(
                text = "Studentized Residuals",
                standoff = 10
              )
            ),
            xaxis = list(
              title = list(
                text = "Timestamps",
                standoff = 10
              )
            ),
            legend = list(
              title = list(text = "")  
            ),
            annotations = list(
              list(
                x = 0,  # Align to the left
                y = 1.22,  # Slightly below the title
                text = paste("MAE:", mae),  
                showarrow = FALSE,  
                xref = "paper",
                yref = "paper",
                align = "left",  # Align text to the left
                font = list(
                  size = 15.5,  # Two sizes smaller than title
                  family = "Arial",  # Match title font
                  color = "black"
                )
              )
            )
          )
        
        
        
        # Create combined plot with HTML layout
        x_plots <- htmltools::tagList(
          htmltools::div(
            style = "display: grid; grid-template-columns: 1fr; max-width: 100%; font-family: Arial, sans-serif;margin: 0; padding: 0; overflow: hidden;", 
           
            htmltools::div(
              style = "grid-column: 1; width: 100%; margin: 0; padding: 0; overflow: hidden;", 
              p1_plotly
            ),
           
            htmltools::div(
              style = "grid-column: 1; width: 100%;margin: 0; padding: 0; overflow: hidden;", 
              p2_plotly
            )
          )
        )
        }
#browser()
        list(centroids_plot = x_plots, mae = mae)
      })
      
      # Store plots in a list
      plot_list <- all_plots
      
      ########### States Plot ###########
      plot_data <- simulation_results %>%
        select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(
          cols = starts_with("Sim_"),
          names_to = "simulation",
          values_to = "value"
        )
      
      summary_data <- simulation_results %>%
        select(time, mean, median, mode)
      
      test_data <- test_data %>%
        mutate(time = simulation_results$time)  # Using time from simulation_results directly
      
      # Create plot
      # Analyze timestamps
      time_analysis <- analyze_timestamps(summary_data, "time")
      axis_settings <- get_axis_breaks(time_analysis)
      
      # Use standardized data
      summary_data <- time_analysis$standardized_data
      if(!is.null(plot_data)) {
        plot_data <- standardize_timestamp(plot_data, "time")
      }
      
      pa <- ggplot() +
        {if(show_simulation) 
          geom_line(data = plot_data,
                    aes(x = time, y = value, group = simulation,
                        color = "Simulations", text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), alpha = 0.4, size=0.4)} +
        geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), 
                  size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
        
        geom_point(data = summary_data, aes(x = time, y = mode, color = "Mode", text = paste("Time:", time, "<br>Mode:", mode)), 
                   size = ifelse(mae_metric == "mode", 1.5, 0.8)) +
        geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), 
                  size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
        geom_point(data = summary_data, aes(x = time, y = mean, color = "Mean", text = paste("Time:", time, "<br>Mean:", mean)), 
                   size = ifelse(mae_metric == "mean", 1.5, 0.8)) +
        geom_line(data = summary_data, aes(x = time, y = median, color = "Median"),
                  size = ifelse(mae_metric == "median", 1.0, 0.5)) +
        geom_point(data = summary_data, aes(x = time, y = median, color = "Median", text = paste("Time:", time, "<br>Median:", median)),
                   size = ifelse(mae_metric == "median", 1.5, 1)) +
        geom_line(data = test_data,
                  aes(x = time, y = Cell.ID, color = "Actual States"), size = 1.0) +
        geom_point(data = test_data,
                   aes(x = time, y = Cell.ID, color = "Actual States", text = paste("Time:", time, "<br>Actual States:", Cell.ID)), 
                   size = 1.5) +  # Ensure points are visible with proper size
        scale_colour_manual(values = c("Simulations" = "darkgray", 
                                       "Median" = "red",
                                       "Mean" = "darkgreen",
                                       "Actual States" = "black",
                                       "Mode" = "#0901FF")) +
        scale_y_continuous(
          limits = c(0, NA),  # Ensure y-axis starts from zero
          breaks = integer_breaks()  # Custom function for integer breaks
        ) +
        theme_minimal() +
        labs(x = "Timestamps",
             y = "States",
             title = paste0(type, ": Ex-Post Actual States vs Predicted States"),
             color = " ") +
        
        {
          if(inherits(summary_data$time, "POSIXct")) {
            # Use the new functions to analyze timestamps and get appropriate breaks
            time_analysis <- analyze_timestamps(summary_data, "time")
            
            # Get axis settings
            axis_settings <- get_axis_breaks(time_analysis)
            
            # Get the regular breaks using our custom function
            all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
            
            scale_x_datetime(
              breaks = all_breaks,
              minor_breaks = summary_data$time,
              date_labels = axis_settings$date_format,
              expand = expansion(mult = 0.02)
            )
          } else {
            # Fallback for non-POSIXct data
            scale_x_continuous(
              expand = expansion(mult = 0.02)
            )
          }
        } +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_plot
      
      
      
      ##############states residual plot###################
      predicted_metric <- summary_data[[mae_metric]]
      residuals_df <- data.frame(test_data[1:length(predicted_metric),], predicted_metric)
      residuals_df$residuals <- (residuals_df$Cell.ID - residuals_df$predicted_metric)
      residuals_sd <- sd(residuals_df$residuals)
      residuals_df$studentized_residuals <- residuals_df$residuals / residuals_sd
      residuals_df$mape_component <- abs((residuals_df$Cell.ID - residuals_df$predicted_metric) / residuals_df$Cell.ID) * 100
      options(scipen = 999)
      mape <- round(mean(residuals_df$mape_component),4)
      mae <- round(mean(abs(residuals_df$residuals)), 4)
      
      if(all(residuals_df$residuals == 0)) {
        residuals_df$studentized_residuals <- 0
      }
      
      pb <- ggplot() +
        geom_line(data = residuals_df, aes(x = time, y = studentized_residuals, color = "Studentized\nResiduals"), size = 0.8) +
        geom_point(data = residuals_df, aes(x = time, y = studentized_residuals, color = "Studentized\nResiduals", text = paste("Time:", time, "<br>Residuals:", round(studentized_residuals,4))), size =1.2) +
        geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
        geom_hline(yintercept = -1, col = "blue", linetype = "dashed") +
        geom_hline(yintercept = 1, col = "blue", linetype = "dashed") +
        scale_color_manual(name = NULL, values = c("Studentized\nResiduals" = "black")) +
        labs(title = paste0(type, ": Ex-Post Studentized Residuals for the ", mae_metric," forecast" ),
             subtitle = paste("MAE:", mae), 
             x = "Timestamps", y = "Studentized Residuals") +
       
        theme_minimal() +
        {
          if(inherits(summary_data$time, "POSIXct")) {
            # Use the new functions to analyze timestamps and get appropriate breaks
            time_analysis <- analyze_timestamps(summary_data, "time")
            
            # Get axis settings
            axis_settings <- get_axis_breaks(time_analysis)
            
            # Get the regular breaks using our custom function
            all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
            
            scale_x_datetime(
              breaks = all_breaks,
              minor_breaks = residuals_df$time,
              date_labels = axis_settings$date_format,
              expand = expansion(mult = 0.02)
            )
          } else {
            # Fallback for non-POSIXct data
            scale_x_continuous(
              expand = expansion(mult = 0.02)
            )
          }
        } +
        theme_plot+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      
      
      if(plot_type == "static"){
        ###########plot display output########
        x_plots <- (pa / pb) +
          patchwork::plot_layout(heights = c(2, 1))
      }else{
      pa_plotly <- plotly::ggplotly(pa, tooltip = "text") %>%
        plotly::layout(
          margin = list(r = 150, b = 50, t = 50), 
          height = 400, 
          yaxis = list(
            title = list(
              text = ("States"),
              standoff = 10
            )
          ),
          xaxis = list(
            title = list(
              text = "Timestamps",
              standoff = 10
            )
          ),
          legend = list(
            title = list(text = "") 
          )
        )
      
      pb_plotly <- plotly::ggplotly(pb, tooltip = "text") %>%
        plotly::layout(
          margin = list(r = 150, b = 50, t = 60),  # Keep enough top margin
          height = 250, 
          yaxis = list(
            title = list(
              text = "Studentized Residuals",
              standoff = 10
            )
          ),
          xaxis = list(
            title = list(
              text = "Timestamps",
              standoff = 10
            )
          ),
          legend = list(
            title = list(text = "")  
          ),
          annotations = list(
            list(
              x = 0,  # Align to the left
              y = 1.22,  # Slightly below the title
              text = paste("MAE:", mae),  
              showarrow = FALSE,  
              xref = "paper",
              yref = "paper",
              align = "left",  # Align text to the left
              font = list(
                size = 15.5,  # Two sizes smaller than title
                family = "Arial",  # Match title font
                color = "black"
              )
            )
          )
        )
      
      # Create combined plot with HTML layout
      x_plots <- htmltools::tagList(
        htmltools::div(
          style = "display: grid; grid-template-columns: 1fr; max-width: 100%; font-family: Arial, sans-serif;",
         
          htmltools::div(
            style = "grid-column: 1; width: 100%;", 
            pa_plotly
          ),
         
          htmltools::div(
            style = "grid-column: 1; width: 100%;", 
            pb_plotly
          )
        )
      )
      }
      states_plots <- list(x_plots, mae =mae)
      
      ########### Return All Plots ###########
      return(list(plot_list, states_plots=states_plots))
      
    } else {
      
      # For ex-ante forecasts
      all_plots <- lapply(name_columns, function(variable_name) {
        # Data manipulation for plotting
        plot_data <- scaled_dfs[[variable_name]] %>%
          select(time, starts_with("Sim_")) %>%
          tidyr::pivot_longer(
            cols = starts_with("Sim_"),
            names_to = "simulation",
            values_to = "value"
          )
        
        summary_data <- scaled_dfs[[variable_name]] %>%
          select(time, mean, median, mode)
        
        
        # Create plot
        # Analyze timestamps
        time_analysis <- analyze_timestamps(summary_data, "time")
        axis_settings <- get_axis_breaks(time_analysis)
        
        # Use standardized data
        summary_data <- time_analysis$standardized_data
        if(!is.null(plot_data)) {
          plot_data <- standardize_timestamp(plot_data, "time")
        }
        p1 <- ggplot() +
          {if(show_simulation) geom_line(data = plot_data, 
                                         aes(x = time, y = value, group = simulation, 
                                             color = "Simulations", text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), 
                                             alpha = 0.4, size = 0.4)} +
          geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), 
                    size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = mode, color = "Mode", text = paste("Time:", time, "<br>Mode ", variable_name, " :", mode)), 
                    size = ifelse(mae_metric == "mode", 1.5, 1.2)) +
          geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), 
                    size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
          geom_point(data = summary_data, aes(x = time, y = mean, color = "Mean", text = paste("Time:", time, "<br>Mean ", variable_name, " :", mean)), 
                    size = ifelse(mae_metric == "mean", 1.5, 1.2)) +
          geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), 
                    size = ifelse(mae_metric == "median", 1.0, 0.5)) +
          geom_point(data = summary_data, aes(x = time, y = median, color = "Median", text = paste("Time:", time, "<br>Median ", variable_name, " :", median)), 
                    size = ifelse(mae_metric == "median", 1.5, 1.2)) +
          scale_colour_manual(values = c("Simulations" = "darkgray", 
                                         "Median" = "red", 
                                         "Mean" = "darkgreen", 
                                         "Mode" = "#0901FF")) +
    
          # {if(inherits(summary_data$time, "POSIXct")) 
          #   theme(axis.text.x = element_text(angle = 0, hjust = 1))
          # } +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          theme_minimal() +
          {
            if(inherits(summary_data$time, "POSIXct")) {
              # Use the new functions to analyze timestamps and get appropriate breaks
              time_analysis <- analyze_timestamps(summary_data, "time")
              
              # Get axis settings
              axis_settings <- get_axis_breaks(time_analysis)
              
              # Get the regular breaks using our custom function
              all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
              
              scale_x_datetime(
                breaks = all_breaks,
                minor_breaks = summary_data$time,
                date_labels = axis_settings$date_format,
                expand = expansion(mult = 0.02)
              )
            } else {
              # Fallback for non-POSIXct data
              scale_x_continuous(
                expand = expansion(mult = 0.02)
              )
            }
          } +
          labs(x = "Timestamps", 
               y = paste0(variable_name, " (raw units)"),
               title = paste0(type, ": Ex-Ante Predicted ", variable_name),
               color = " ") +
          theme_plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        
        
        if(plot_type == "static"){
          p1 <- p1
        }else{
        p1 <- plotly::ggplotly(p1, tooltip = "text")
        }
        return(p1)
      })
      
      # Store plots in a list
      plot_list <- all_plots
      
      
      # States plot
      plot_data <- simulation_results %>%
        select(time, starts_with("Sim_")) %>%
        tidyr::pivot_longer(
          cols = starts_with("Sim_"),
          names_to = "simulation",
          values_to = "value"
        )
      summary_data <- simulation_results %>%
        select(time, mean, median, mode)
      
      
      # Create plot
      # Analyze timestamps
      time_analysis <- analyze_timestamps(summary_data, "time")
      axis_settings <- get_axis_breaks(time_analysis)
      
      # Use standardized data
      summary_data <- time_analysis$standardized_data
      if(!is.null(plot_data)) {
        plot_data <- standardize_timestamp(plot_data, "time")
      }
      
      p2 <- ggplot() +
        {if(show_simulation) geom_line(data = plot_data, 
                                       aes(x = time, y = value, group = simulation, 
                                           color = "Simulations", text = paste("Time:", time, "<br>Value:", value, "<br>Simulation:", simulation)), 
                                           alpha = 0.4, size = 0.3)} +
        geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), 
                  size = ifelse(mae_metric == "mode", 1.0, 0.4)) +
        geom_point(data = summary_data, aes(x = time, y = mode, color = "Mode", text = paste("Time:", time, "<br>Mode:", mode)), 
                  size = ifelse(mae_metric == "mode", 1.5, 1.2)) +
        geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), 
                  size = ifelse(mae_metric == "mean", 1.0, 0.4)) +
        geom_point(data = summary_data, aes(x = time, y = mean, color = "Mean", text = paste("Time:", time, "<br>Mean:", mean)), 
                  size = ifelse(mae_metric == "mean", 1.5, 1.2)) +
        geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), 
                  size = ifelse(mae_metric == "median", 1.0, 0.5)) +
        geom_point(data = summary_data, aes(x = time, y = median, color = "Median", text = paste("Time:", time, "<br>Median:", median)), 
                  size = ifelse(mae_metric == "median", 1.5, 1.2)) +
        scale_colour_manual(values = c("Simulations" = "darkgray", 
                                       "Median" = "red",
                                       "Mean" = "darkgreen",
                                       "Mode" = "#0901FF")) +
        scale_y_continuous(
          limits = c(0, NA),  # Ensure y-axis starts from zero
          breaks = integer_breaks()  # Custom function for integer breaks
        ) +      
        # {if(inherits(summary_data$time, "POSIXct")) 
        #   theme(axis.text.x = element_text(angle = 0, hjust = 1))
        # } +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_minimal() +
        {
          if(inherits(summary_data$time, "POSIXct")) {
            # Use the new functions to analyze timestamps and get appropriate breaks
            time_analysis <- analyze_timestamps(summary_data, "time")
            
            # Get axis settings
            axis_settings <- get_axis_breaks(time_analysis)
            
            # Get the regular breaks using our custom function
            all_breaks <- get_regular_breaks(summary_data$time, time_analysis$interval_type)
            
            scale_x_datetime(
              breaks = all_breaks,
              minor_breaks = summary_data$time,
              date_labels = axis_settings$date_format,
              expand = expansion(mult = 0.02)
            )
          } else {
            # Fallback for non-POSIXct data
            scale_x_continuous(
              expand = expansion(mult = 0.02)
            )
          }
        } +
        labs(x = "Timestamps",
             y = "States",
             title = paste0(type, ": Ex-Ante Predicted States"),
             color = " ") +
        theme_plot +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      
      
      if(plot_type == "static"){
        p2 <- p2
      }else{
        p2 <- plotly::ggplotly(p2, tooltip = "text")
      }

      return(list(centroids_plot = plot_list, states_plots = p2))
    }
  }
}