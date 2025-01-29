#' @name plotAnimatedFlowmap
#' @title Generating flow maps and animations based on transition probabilities
#' @description This is the main function for generating flow maps and animations based on transition probabilities
#' including self states and excluding self states.
#' Flow maps are a type of data visualization used to represent the transition probability of different states. 
#' Animations are the gifs used to represent the movement of data through the cells. 
#' @param hvt_model_output List. Output from a trainHVT function.
#' @param transition_probability_df List. Output from getTransitionProbability function
#' @param df Data frame. The input dataframe should contain two columns, 
#' cell ID from scoreHVT function and time stamp of that dataset.
#' @param animation Character. Type of animation ('state_based', 'time_based', 'All' or  NULL)
#' @param flow_map Character. Type of flow map ('self_state', 'without_self_state', 'All' or NULL)
#' @param fps_time Numeric. A numeric value for the frames per second of the time transition gif.
#' (Must be a numeric value and a factor of 100). Default value is 1.
#' @param fps_state Numeric. A numeric value for the frames per second of the state transition gif.
#' (Must be a numeric value and a factor of 100). Default value is 1.
#' @param time_duration Numeric. A numeric value for the duration of the time transition gif.
#' Default value is 2.
#' @param state_duration Numeric. A numeric value for the duration of the state transition gif.
#' Default value is 2.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing time stamps
#' @return A list of flow maps and animation gifs.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>, Vishwavani <vishwavani@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} \cr \code{\link{scoreHVT}} \cr \code{\link{getTransitionProbability}}
#' @keywords Timeseries_Analysis
#' @importFrom magrittr %>%
#' @examples
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#'                       DAX = EuStockMarkets[, "DAX"],
#'                       SMI = EuStockMarkets[, "SMI"],
#'                       CAC = EuStockMarkets[, "CAC"],
#'                       FTSE = EuStockMarkets[, "FTSE"])
#' hvt.results<- trainHVT(dataset,n_cells = 60, depth = 1, quant.err = 0.1,
#'                        distance_metric = "L1_Norm", error_metric = "max",
#'                        normalize = TRUE,quant_method = "kmeans")
#' scoring <- scoreHVT(dataset, hvt.results)
#' cell_id <- scoring$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' table <- getTransitionProbability(dataset, cellid_column = "cell_id",time_column = "time_stamp")
#' plots <- plotAnimatedFlowmap(hvt_model_output = hvt.results, transition_probability_df = table,
#' df = dataset, animation = 'All', flow_map = 'All',fps_time = 1,fps_state =  1,time_duration = 3,
#' state_duration = 3,cellid_column = "cell_id", time_column = "time_stamp")
#' @export plotAnimatedFlowmap

plotAnimatedFlowmap <- function(hvt_model_output, transition_probability_df, df, 
                                animation = "All", flow_map = "All", 
                                fps_time = 1, fps_state = 1, time_duration = 2, 
                                state_duration = 2, cellid_column, time_column) {
  
  
  ##for cran warnings
  CircleSize  <- Current_State <- Next_State <- Transition_Probability <- Cumulative_Probability <- Relative_Frequency <- latency <- NULL
  
  valid_animation <- c("All", "state_based", "time_based", NULL)
  valid_flowmap <- c("All", "self_state", "without_self_state", NULL)
  
  if (!is.null(animation) && !(animation %in% valid_animation)) {
    stop("Invalid animation argument. Must be one of: 'All', 'state_based', 'time_based', or NULL")
  }
  
  if (!is.null(flow_map) && !(flow_map %in% valid_flowmap)) {
    stop("Invalid flow_map argument. Must be one of: 'All', 'self_state', 'without_self_state', or NULL") 
  }
  
 
   ##for cran warnings, initializing empty vectors for these variables.
  segment_len <- grp <- colour<-order_map<- Frequency <-Timestamp<-y2 <-x2 <-y1.y<- x1.y<- y1 <-x1<-label<-NULL
  

  
  # Rename columns for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  
  ###########centroid - x and y coordinates################
  hvt_res1 <- hvt_model_output[[2]][[1]]$`1`
  hvt_res2 <- hvt_model_output[[3]]$summary$Cell.ID
  coordinates_value1 <- lapply(1:length(hvt_res1), function(x) {
    centroids1 <- hvt_res1[[x]]
    coordinates1 <- centroids1$pt
  })
  cellID_coordinates <- do.call(rbind.data.frame, coordinates_value1)
  colnames(cellID_coordinates) <- c("x", "y")
  cellID_coordinates$Cell.ID <- hvt_res2
  # Subset the arrow starting coordinates based on the order
  current_state_data <- dplyr::arrange(cellID_coordinates, Cell.ID)
  colnames(current_state_data) <- c("x1", "y1", "Cell.ID")
  
  ################################################################
  #####Function to get highest state and probability excluding self-state
  get_second_highest <- function(df) {
    df$Next_State <- as.integer(df$Next_State)
    # Remove self transitions first
    df <- df[df$Next_State != df$Current_State[1], ]
    
    if(nrow(df) > 0) {
      # Get the highest probability from remaining transitions
      max_probability_row <- df[which.max(df$Transition_Probability), ]
      return(data.frame(
        Next_State = max_probability_row$Next_State,
        Probability = max_probability_row$Transition_Probability
      ))
    }
    return(data.frame(Next_State = 0, Probability = 0))
  }
  
  grouped_df <- split(transition_probability_df, transition_probability_df$Current_State)
  second_highest_states_list <- lapply(grouped_df, get_second_highest)
  second_state_df <- do.call(rbind, second_highest_states_list)
  ##########################################################
  
  
  
  ######################################################################
  # Function to get the highest probability state and probability
  get_highest_probability <- function(df) {
    df$Next_State <- as.integer(df$Next_State)
    # Get max probability
    max_prob <- max(df$Transition_Probability)
    # Filter rows with max probability
    max_prob_rows <- df[df$Transition_Probability == max_prob, ]
    
    # If current state exists in max probability rows, select it
    current_state <- df$Current_State[1]
    if(current_state %in% max_prob_rows$Next_State) {
      selected_row <- max_prob_rows[max_prob_rows$Next_State == current_state, ]
    } else {
      selected_row <- max_prob_rows[1, ]
    }
    
    return(data.frame(Next_State = selected_row$Next_State, 
                      Probability = selected_row$Transition_Probability))
  }
  
  # Apply the function to each data frame in 'transition_probability_df'
  grouped_df <- split(transition_probability_df, transition_probability_df$Current_State)
  highest_probability_states_list <- lapply(grouped_df, get_highest_probability)
  
  # Combine the results into a single data frame
  first_state_df <- do.call(rbind, highest_probability_states_list)
  ###################################################################################
  
  ############## Dataframe for second_highest state (without self state)
  merged_df1 <- cbind(current_state_data, second_state_df)
  merged_df1 <- merged_df1 %>%
    left_join(dplyr::select(merged_df1, Cell.ID, x1, y1), by = c("Next_State" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df1) <- c("x1", "y1", "Cell.ID", "Next_State", "Probability", "x2", "y2")
  
  # Dataframe for highest state  (with self state)
  merged_df2 <- cbind(current_state_data, first_state_df)
  merged_df2 <- merged_df2 %>%
    left_join(dplyr::select(merged_df2, Cell.ID, x1, y1), by = c("Next_State" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df2) <- c("x1", "y1", "Cell.ID", "Next_State", "Probability", "x2", "y2")
  merged_df2$Probability <- round(merged_df2$Probability, digits = 3)
  
  # Self-state Plot
  prob1 <- merged_df2$Probability 
  cellID_coordinates$prob1 <- prob1
  
  if (!is.null(flow_map)) {
    if(flow_map %in% c('self_state', 'All')) {

      # Get min and max probabilities
      min_prob <- min(merged_df2$Probability)
      max_prob <- max(merged_df2$Probability)
      
      # Create breaks based on 30% quantiles
      custom_breaks <- stats::quantile(merged_df2$Probability, probs = seq(0, 1, by = 0.3))
      custom_breaks[1] <- min_prob - 0.001
      
      # Assign circle sizes
      merged_df2$CircleSize <- as.numeric(cut(merged_df2$Probability, 
                                              breaks = custom_breaks, 
                                              labels = seq(1, length(custom_breaks) - 1)))
      
      # Handle NA values
      merged_df2$CircleSize <- ifelse(is.na(merged_df2$CircleSize), 
                                      max(merged_df2$CircleSize, na.rm = TRUE), 
                                      merged_df2$CircleSize)
      
      # Create legend sizes dynamically based on number of unique categories
      n_categories <- length(unique(merged_df2$CircleSize))
      legend_size <- 2.6 * seq_len(n_categories)
      
      # Create breaks for legend
      breaks <- as.numeric(custom_breaks)
      breaks[1] <- breaks[1] + 0.001
      
      # Generate legend labels
      legend_labels <- paste0(format(round(breaks[-length(breaks)], 3), nsmall = 3), 
                              " to ", 
                              format(round(breaks[-1], 3), nsmall = 3))

      # Create the plot
      self_state_plot <- ggplot2::ggplot() +
        ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y, color = prob1), size = 0.9) +
        ggplot2::geom_point(data = merged_df2, 
                            aes(x = x1, y = y1, size = CircleSize),
                            shape = 1,  
                            color = "blue") +
        ggplot2::geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
        scale_color_gradient(low = "black", high = "black",
                             name = "Probability",
                             breaks = breaks[-length(breaks)],  
                             labels = legend_labels) +
        scale_size(range = c(2, 10)) +
        labs(title = "State Transitions: Circle size based on Transition Probability",
             subtitle = "considering self state transitions",
             x = "x-coordinates",  
             y = "y-coordinates") +
        guides(color = guide_legend(title = "Transition\nProbability", 
                                    override.aes = list(shape = 21, size = legend_size, color = "blue")), 
               fill = guide_legend(title = "Probability", override.aes = list(color = "blue", size = legend_size)),
               size = "none") +
        theme_minimal()
    }
    if(flow_map %in% c('without_self_state', 'All')) {

      # Initial transition matrix with removal of self-transitions
      trans_prob_matx  <- getTransitionProbability(df = df,
                                                                cellid_column = "Cell.ID",
                                                                time_column = "t",
                                                                type = "with_self_state")
      
      # Keep track of states that have only single transition AND it's to themselves
      single_transition_states <- trans_prob_matx %>%
        group_by(Current_State) %>%
        filter(n() == 1 & Current_State == Next_State) %>%
        ungroup()
      
      # Remove self-transitions for states with multiple transitions 
      trans_prob_matx <- trans_prob_matx %>%
        group_by(Current_State) %>%
        filter(!(n() == 1 & Current_State == Next_State)) %>%
        filter(Current_State != Next_State) %>%
        ungroup()
      
      # Calculate probabilities
      trans_prob_matx <- trans_prob_matx %>%
        group_by(Current_State) %>%
        mutate(
          Transition_Probability = round((Relative_Frequency / sum(Relative_Frequency)), 4),
          Cumulative_Probability = cumsum(Transition_Probability),
          Cumulative_Probability = if_else(row_number() == n(), 1, Cumulative_Probability)
        ) %>% 
        ungroup()
      
      # Combine with single self-transition states
      trans_prob_matx <- rbind(trans_prob_matx, single_transition_states)
      filtered_trans_prob <- trans_prob_matx %>%
        group_by(Current_State) %>%
        arrange(desc(Transition_Probability), Next_State) %>%
        slice_head(n = 1) %>%
        ungroup()

      third_df <- filtered_trans_prob %>% dplyr::select(c(Next_State, Transition_Probability))
 
  # Dataframe for second_highest state
  merged_df3 <- cbind(current_state_data, third_df)
  merged_df3 <- merged_df3 %>%
    left_join(dplyr::select(merged_df3, Cell.ID, x1, y1), by = c("Next_State" = "Cell.ID")) %>%
    dplyr::mutate(x2 = x1.y, y2 = y1.y) %>%
    dplyr::select(-x1.y, -y1.y)
  colnames(merged_df3) <- c("x1", "y1", "Cell.ID", "Next_State", "Probability", "x2", "y2")
  merged_df3$Probability <- round(merged_df3$Probability, digits = 1)
  
 
  # NEW CODE FOR PLOT LEGEND
  segment_length <- function(p) {
    case_when(
      p <= 0.3 ~ 0.2,
      p <= 0.6 ~ 0.4,
      p <= 0.8 ~ 0.7,
      TRUE ~ 0.9
    )
  }

  # Calculate segment lengths
  merged_df3$segment_len <- segment_length(merged_df3$Probability)

  # Calculate relative ranges to position legend consistently
  y_range <- range(merged_df3$y1)
  x_range <- range(merged_df3$x1)
  plot_width <- diff(x_range)
  plot_height <- diff(y_range)
  
  # Set fixed positions relative to plot dimensions
  y1_l <- max(y_range) - 0.1*plot_height  
  y2_l <- max(y_range) - 0.15*plot_height
  
  x1_l <- max(x_range) + 0.15*plot_width
  x2_l <- max(x_range) + 0.2*plot_width 
  x3_l <- max(x_range) + 0.35*plot_width
  
  y1_l <- mean(y_range)  # Center vertically
  x1_l <- max(x_range) + 0.05*plot_width  # Slightly right of plot
  
  # Dynamically scale arrow lengths based on plot dimensions 
  arrow_scale <- min(plot_width, plot_height) / 20  # Scale factor
  
  # Check actual probability ranges in data
  prob_ranges <- case_when(
    merged_df3$Probability <= 0.3 ~ "0 to 0.3",
    merged_df3$Probability > 0.3 & merged_df3$Probability <= 0.6 ~ "0.4 to 0.6",
    merged_df3$Probability > 0.6 & merged_df3$Probability <= 0.8 ~ "0.7 to 0.8", 
    merged_df3$Probability > 0.8 ~ "0.9 to 1"
  )
  
  unique_ranges <- rev(unique(prob_ranges))
  
  create_legend <- function(x1_l, x2_l, x3_l, y1_l, y2_l) {
    labels <- unique_ranges  # Use actual ranges found in data
    arrow_lengths <- seq(1, length(labels)*2 , by=1) * arrow_scale
    spacing <- plot_height / 20
    
    legend_elements <- list(
      annotate("text", x = x1_l + 2*arrow_scale, y = y1_l + spacing, 
               label = "Transition\nProbability", size = 4)
    )
    
    for(i in seq_along(labels)) {
      y_pos <- y1_l - i * spacing
      
      legend_elements[[length(legend_elements) + 1]] <- 
        annotate("segment", 
                 x = x1_l, 
                 xend = x1_l + arrow_lengths[i],
                 y = y_pos, 
                 yend = y_pos,
                 color = "blue",
                 arrow = arrow(length = unit(1.5, "mm"), type = "open"))
      
      legend_elements[[length(legend_elements) + 1]] <- 
        annotate("text", 
                 x = x1_l + arrow_lengths[i] + arrow_scale/2,
                 y = y_pos, 
                 label = labels[i], 
                 size = 3,
                 hjust = 0)
    }
    return(legend_elements)
  }
  # Generate legend
  annotate_list <- create_legend(
    x1_l = x1_l,
    x2_l = x2_l,
    x3_l = x3_l,
    y1_l = y1_l,
    y2_l = y2_l
  )
  
  # Create the plot
  arrow_flow_map <- ggplot(merged_df3, aes(x = x1, y = y1)) +
    geom_point(color = "black", size = 0.9) +
    geom_text(aes(label = Cell.ID), vjust = -1, size = 3) +
    geom_segment(aes(xend = x1 + (x2 - x1) * segment_len,
                     yend = y1 + (y2 - y1) * segment_len),
                 arrow = arrow(length = unit(merged_df3$segment_len * 3, "mm")),
                 color = "blue") +
    labs(title = "State Transitions: Arrow size based on Transition Probability",
         subtitle = "without considering self state transitions",
         x = "x-coordinates",
         y = "y-coordinates") +
    theme_minimal()+
    scale_x_continuous(limits = c(min(merged_df3$x1), max(merged_df3$x1) + plot_width/3))
  
  # Add legend annotations
  for(annotation in annotate_list) {
    arrow_flow_map <- arrow_flow_map + annotation
  }
  
    }
  }  
    if (!is.null(animation)) {
      if(animation %in% c('time_based', 'All')) {
    sampled_df <- df %>% dplyr::select(Cell.ID, Timestamp)
    anime_data <- merge(sampled_df, cellID_coordinates, by = "Cell.ID", all.x = TRUE) %>%
      dplyr::arrange(Timestamp) %>%
      dplyr::select(-prob1) %>%
      dplyr::group_by(grp = cumsum(Cell.ID != lag(Cell.ID, default = first(Cell.ID)))) %>% 
      dplyr::mutate(colour = 2 - row_number() %% 2) %>%
      dplyr::ungroup() %>%
      dplyr::select(-grp) 


    if (!inherits(df$Timestamp, c("POSIXct", "POSIXt", "numeric"))) {
      stop("Accepted Timestamp data types: POSIXct, POSIXt and numeric")
    }
    
    if (!inherits(df$Timestamp, c("POSIXct", "POSIXt", "numeric"))) {
      stop("Accepted Timestamp data types: POSIXct, POSIXt and numeric")
    }
    
    if (inherits(anime_data$Timestamp, c("POSIXct", "POSIXt"))) {
      # Calculate time differences in days
      time_diffs <- as.numeric(difftime(lead(anime_data$Timestamp), 
                                        anime_data$Timestamp, 
                                        units = "days"))
      
      # Determine frequency
      med_diff <- stats::median(time_diffs, na.rm = TRUE)
      if (med_diff >= 365) {
        freq_unit <- "years"
        scale_factor <- 1/365
      } else if (med_diff >= 28) {
        freq_unit <- "months"
        scale_factor <- 1/30.44
      } else if (med_diff >= 1) {
        freq_unit <- "days"
        scale_factor <- 1
      } else if (med_diff >= 1/24) {
        freq_unit <- "hours"
        scale_factor <- 24
      } else {
        freq_unit <- "mins"
        scale_factor <- 24*60
      }
      
      anime_data <- anime_data %>%
        arrange(Timestamp) %>%
        mutate(
          latency = as.numeric(difftime(lead(Timestamp), Timestamp, units = "days")) * scale_factor,
          freq_unit = freq_unit
        ) %>%
        ungroup()
      
      format_string <- switch(freq_unit,
                              "years" = "%Y",
                              "months" = "%Y-%m",
                              "days" = "%Y-%m-%d",
                              "hours" = "%Y-%m-%d %H:00",
                              "mins" = "%Y-%m-%d %H:%M"
      )
      
      subtitle_text <- paste0(
        "\n\ntime(t): {format(frame_time, format_string)}\n",
        "Latency: {round(anime_data$latency[findInterval(as.numeric(frame_time), 
                                   as.numeric(anime_data$Timestamp))], 2)} ",
        freq_unit
      )
    } else if (is.numeric(anime_data$Timestamp)) {
      anime_data <- anime_data %>%
        arrange(Timestamp) %>%
        mutate(latency = lead(Timestamp) - Timestamp) %>%
        mutate(latency = formatC(latency, format = "f", digits = 5)) %>%
        ungroup()
      
      subtitle_text <- paste0(
        "\n\ntime(t): {round(frame_time, 3)} seconds\n",
        "Latency: {anime_data$latency[frame]} seconds"
      )
    }

#browser()    
    dot_anim <- ggplot2::ggplot(anime_data, aes(x = x, y = y)) +
      ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), color = "black", size = 1) +
      ggplot2::geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3) +
      ggplot2::geom_point(aes(x = x, y = y, color = ifelse(colour == 1, "Active state at t", " ")), alpha = 0.7, size = 5) +
      scale_color_manual(values = c("Active state at t" = "red", " " = "white")) +
      theme_minimal() +
      labs(x = "x-coordinates", 
           y = "y-coordinates", 
           color = "Time Transition",
           title = "Animation showing state transitions considering self state transitions",
           subtitle = subtitle_text) +
      theme(plot.subtitle = element_text(margin = margin(t = 20))) +
      gganimate::transition_time(Timestamp) +
      gganimate::shadow_wake(wake_length = 0.05, alpha = FALSE, wrap = FALSE)
    
    time_animation <- gganimate::animate(dot_anim, 
                              fps = fps_time, 
                              duration = time_duration, 
                              renderer = gganimate::gifski_renderer())
    
  } 

   
      if(animation %in% c('state_based', 'All')) {
  ### Animation based on next state
  df_a <- df %>%group_by(Cell.ID) %>%dplyr::mutate(Frequency = with(rle(Cell.ID), rep(lengths, lengths)))
  state_data <- df_a %>%group_by(grp = cumsum(c(TRUE, diff(Cell.ID) != 0))) %>% slice(n())
  order <- unique(state_data$Cell.ID)
  anime_df <- merged_df1[order, ]
  anime_df$order_map <- 1:nrow(anime_df)
  anime_df$label <- rep("Successive states", nrow(anime_df))


  #NEWLY ADDED FOR ARROW HEAD SIZE
  x <- anime_df$x1
  y <- anime_df$y1
  xend <- anime_df$x1 + (anime_df$x2 - anime_df$x1) * 0.5
  yend <- anime_df$y1 + (anime_df$y2 - anime_df$y1) * 0.5
  segment_lengths <- sqrt((anime_df$x2 - anime_df$x1)^2 + (anime_df$y2 - anime_df$y1)^2)
  
  # Scale arrow head length based on segment length
  arrow_head_length <- case_when(
    segment_lengths <= 5 ~ 1,
    segment_lengths <= 10 ~ 2,
    segment_lengths <= 20 ~ 3,
    TRUE ~ 4
  )

  arrow_anim <- ggplot2::ggplot(anime_df, aes(x = x1, y = y1)) +
    geom_segment(data = anime_df, mapping = aes(x = x1, y = y1, xend = x1 + (x2 - x1) * 0.5, yend = y1 + (y2 - y1) * 0.5, color = label),
                 arrow = arrow(length = unit(arrow_head_length, "mm")), show.legend = TRUE) +
    ggplot2::geom_point(data = cellID_coordinates, aes(x = x, y = y), size = 1, show.legend = FALSE) +
    geom_text(data = cellID_coordinates, aes(x = x, y = y, label = Cell.ID), vjust = -1, size = 3 ) +
    scale_color_manual(values = c("Successive states" = "blue")) + 
    labs(x = "x-coordinates", y = "y-coordinates", color = "State Transition") + theme_minimal()
  
  animation1 <- arrow_anim + gganimate::transition_states(order_map, wrap = FALSE) + gganimate::shadow_mark() +
    labs(title = "Animation showing state transitions",
         subtitle = "without considering self-state transitions")
  
  state_animation <- gganimate::animate(animation1, fps = fps_state, duration = state_duration, renderer = gganimate::gifski_renderer())
    } 
   
    }
  
  plots <- list()
  
  if (!is.null(flow_map)) {
    if(flow_map %in% c('self_state', 'All')) {
      plots$self_state <- self_state_plot
    }
    if(flow_map %in% c('without_self_state', 'All')) {
      plots$without_self_state <- arrow_flow_map
    }
  }
  
  if (!is.null(animation)) {
    if(animation %in% c('time_based', 'All')) {
      plots$time_based <- time_animation
    }
    if(animation %in% c('state_based', 'All')) {
      plots$state_based <- state_animation
    } 
  }
  
  return(plots)
  
}