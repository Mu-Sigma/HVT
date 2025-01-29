
msm_plots <- function(simulation_results, centroid_data,centroid_2d_points, actual_data, 
                       time_column = 't', state_time_data ,forecast_type,
                       n_ahead_ante, type,raw_dataset, show_simulation,
                       mae_metric=mae_metric) {
  
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


  ######### joining the dataframe ##########
  centroid_dataframe <- cbind(centroid_data, Cell.ID = centroid_2d_points$Cell.ID)
  
  ########### Function to generate the predicted dataframe based on coordinate type #########
  generate_predicted_df <- function(coord) {
    centroid_map <- centroid_dataframe %>%
      select(Cell.ID, coord)
    
    replace_with_value <- function(column) {
      centroid_map[[coord]][match(column, centroid_map$Cell.ID)]
    }
    
    sim_df <- simulation_results_wo_statistics %>%
      select(starts_with("Sim")) %>%
      mutate_all(replace_with_value) %>%
      rename_with(~ paste0(., "_", coord), everything())
    
    simulation_results_wo_statistics %>%
      select(time) %>%
      bind_cols(sim_df)
  }
  
  predicted_dfs <- lapply(name_columns, generate_predicted_df)
  names(predicted_dfs) <- name_columns
  
  ########### Scaling the predicted centroids ##########
  scaled_dfs <- lapply(names(predicted_dfs), function(name) {
    scale_df <- predicted_dfs[[name]] %>%
      dplyr::select(-time) %>%
      mutate(across(starts_with("Sim_"), ~ (. *  sd_raw[1, name]) + mean_raw[1, name])) 

    scale_df <- scale_df %>%
      rowwise() %>%
      mutate(
        mean = round(mean(as.numeric(c_across(starts_with("Sim_"))), na.rm = TRUE), 4),
        median = round(stats::median(as.numeric(c_across(starts_with("Sim_"))), na.rm = TRUE), 4),
        mode = {
          vals <- as.numeric(c_across(starts_with("Sim_")))
          tbl <- table(vals)
          mode_val <- if(length(tbl) == 0) NA else as.numeric(names(tbl)[which.max(tbl)])
          mode_val <- round(mode_val, 4)
          mode_val
        }
      ) %>%
      ungroup() %>%
      as.data.frame()
    
    predicted_df <- cbind(time = predicted_dfs[[name]]$time, scale_df)
    return(predicted_df)
  })
  names(scaled_dfs) <- names(predicted_dfs)
  
  
  
  
  
  ########### Ex-Post Actual vs Predicted Plot ##########
  if (forecast_type == "ex-post") {
    
  
    test_dataset <- actual_data
    ####################
    
    test_data <- tail(state_time_data, nrow(test_dataset))
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
      

      # Actual vs Predicted Plot (p1)
      p1 <- ggplot() +
        {if(show_simulation) geom_line(data = plot_data, aes(x = time, y = value, group = simulation, color = "Simulations"), alpha = 0.4, size = 0.4)} +
        
       # geom_line(data = plot_data, aes(x = time, y = value, group = simulation, color = "Simulations"), alpha = 0.4, size = 0.4) +
        geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), size = 0.4) +
        geom_line(data = actual_raw_dfs[[variable_name]], aes(x = time, y = !!sym(actual_col_name), color = "Actual"), size = 0.5) +
        geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), size = 0.4) +
        geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), size = 0.5) +
        scale_colour_manual(values = c("Simulations" = "darkgray", "Median" = "red", "Mean" = "darkgreen", "Actual" = "black", "Mode" = "#0901FF")) +
        theme_minimal() +
        labs(x = "Timestamps", y = paste0("Centroid ", variable_name), title = paste0(type, ": Ex-Post- Actual ",variable_name, " vs Predicted ",variable_name),
             color = " ") 

      # Residuals Plot (p2)
      p2 <- ggplot() +
        geom_line(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals"), size = 0.8) +
        geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
        geom_hline(yintercept = -1, col = "blue", linetype = "dashed") +
        geom_hline(yintercept = 1, col = "blue", linetype = "dashed") +
        scale_color_manual(name = NULL, labels = function(x) gsub("\n", "\n", x), values = "black") +
        labs(title = paste0(type, ": Ex-Post- Studentized Residuals of ", variable_name),
             subtitle = paste("MAE:", mae), 
             x = "Timestamps", y = "Studentized Residuals") +
        theme_minimal() +
        theme(legend.position = "right", legend.key.width = unit(0.5, "cm"))
      
      # Combine the plots
      x_plots <- (p1 / p2) + 
        patchwork::plot_layout(heights = c(2, 1))
      
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
      {if(show_simulation) geom_line(data = plot_data,aes(x = time, y = value, group = simulation,color = "Simulations"), alpha = 0.4, size=0.4)} +
      geom_line(data = summary_data,aes(x = time, y = mode,color = "Mode"),size = 0.4) +
      geom_line(data = summary_data,aes(x = time, y = mean,color = "Mean"), size = 0.4) +
      geom_line(data = summary_data,aes(x = time, y = median,color = "Median"), size = 0.5) +
      geom_line(data = test_data,aes(x = t, y = Cell.ID,color = "Actual States"), size = 0.5) +
      scale_colour_manual(values = c("Simulations"="darkgray", "Median"="red",
                                     "Mean" =  "darkgreen","Actual States"= "black","Mode"="#0901FF")) +
      theme_minimal() +labs(x = "Timestamps",y = "States",title = paste0(type, ": Ex-Post- Actual States vs Predicted States"),color = " ")+
      theme(legend.position = "right") 
    
    
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
    
    pb <- ggplot() +
      geom_line(data = residuals_df, aes(x = t, y = studentized_residuals, color = "Studentized\nResiduals"), size = 0.8) +
      geom_hline(yintercept=0, col="black", linetype="dashed") +
      geom_hline(yintercept=-1, col="blue", linetype="dashed") +
      geom_hline(yintercept=1, col="blue", linetype="dashed") +
      scale_color_manual(name = NULL, labels = function(x) gsub("\n", "\n", x), values = "black") +
      labs(title =paste0(type, ": Ex-Post- Studentized Residuals of States"),
           subtitle = paste("MAE:", mae), 
           x = "Timestamps", y = "Studentized Residuals") +
      theme_minimal() + theme(legend.position = "right",
                              legend.key.width = unit(0.5, "cm"))
    
    ###########plot display output########
    states_plots <- (pa / pb) + 
      patchwork::plot_layout(heights = c(2, 1))
    
    states_plots <- list(states_plots=states_plots, mae =mae)
    
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
        {if(show_simulation)geom_line(data = plot_data, aes(x = time, y = value, group = simulation, color = "Simulations"), alpha = 0.4, size = 0.4)} +
        geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), size = 0.4) +
        geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), size = 0.4) +
        geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), size = 0.5) +
        scale_colour_manual(values = c("Simulations" = "darkgray", "Median" = "red", 
                                       "Mean" = "darkgreen", "Mode" = "#0901FF")) +
        # scale_y_continuous(breaks = function(x) round(seq(from = floor(x[1]), 
        #                                                   to = ceiling(x[2]), 
        #                                                   length.out = 10))) +
        {if(inherits(summary_data$time, "POSIXct")) 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } +
        theme_minimal() +
        labs(x = "Time", 
             y = paste0("Centroid ", variable_name), 
             title = paste0(type, ": Ex-Ante- Predicted ", variable_name),
             color = " ")
      
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
      {if(show_simulation)geom_line(data = plot_data, aes(x = time, y = value, group = simulation, color = "Simulations"), alpha = 0.4, size = 0.3)} +
      geom_line(data = summary_data, aes(x = time, y = mode, color = "Mode"), size = 0.4) +
      geom_line(data = summary_data, aes(x = time, y = mean, color = "Mean"), size = 0.4) +
      geom_line(data = summary_data, aes(x = time, y = median, color = "Median"), size = 0.5) +
      scale_colour_manual(values = c("Simulations" = "darkgray", "Median" = "red",
                                     "Mean" = "darkgreen", "Mode" = "#0901FF")) +
      {if(inherits(summary_data$time, "POSIXct")) 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } +
      theme_minimal() +
      labs(x = "Time",
           y = "States",
           title = paste0(type, ": Ex-Ante- Predicted States"),
           color = " ") +
      theme(legend.position = "right")

    
    return(list(centroids_plot = plot_list, states_plots = p2))
  }
 
}

  