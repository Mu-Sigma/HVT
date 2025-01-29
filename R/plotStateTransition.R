#' @name plotStateTransition
#' @title Creating State Transition Plot 
#' @description This is the main function to create a state transition plot from a data frame.
#' A state transition plot is a type of data visualization used to represent 
#' the changes or transitions in states over time for a given system. 
#' State refers to a particular condition or status of a cell at a specific point in time. 
#' Transition refers to the change of state for a cell from one condition to another over time. 
#' @param df Data frame. The Input data frame should contain two columns. 
#' Cell ID from scoreHVT function and time stamp of that dataset.
#' @param sample_size Numeric. An integer indicating the fraction of the data frame to visualize in the plot.
#' Default value is 0.2
#' @param line_plot Logical. A logical value indicating to create a line plot. Default value is NULL.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing time stamps.
#' @param v_intercept Numeric. A numeric value indicating the time stamp to draw a vertical line on the plot.
#' @param time_periods List. A list of vectors, each containing start and end times for highlighting time periods.
#' @return A plotly object representing the state transition plot for the given data frame.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>, Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords Timeseries_Analysis
#' @importFrom magrittr %>%
#' @examples
#' dataset <- data.frame(date = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#'
#' hvt.results<- trainHVT(dataset,n_cells = 60, depth = 1, quant.err = 0.1,
#'                        distance_metric = "L1_Norm", error_metric = "max",
#'                        normalize = TRUE,quant_method = "kmeans")
#' scoring <- scoreHVT(dataset, hvt.results)
#' cell_id <- scoring$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$date
#' dataset <- data.frame(cell_id, time_stamp)
#' plotStateTransition(dataset, sample_size = 1, cellid_column = "cell_id",time_column = "time_stamp")
#' @export plotStateTransition

plotStateTransition <- function(df, sample_size = NULL, line_plot = NULL, 
                                cellid_column, time_column, v_intercept = NULL,
                                time_periods = NULL) { 
  ## For CRAN warnings, initializing empty vectors for these variables.
  Timestamp <- Frequency <- Next_State <- NULL
  
  # Rename column names for Time and Cell for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  
  # Validate time_periods parameter structure
  if (!is.null(time_periods)) {
    if (!is.list(time_periods) || !all(sapply(time_periods, length) == 2)) {
      stop("time_periods must be a list of vectors, each containing start and end times")
    }
    
    # Convert time_periods to match Timestamp data type
    if (inherits(df$Timestamp, "POSIXct")) {
      time_periods <- lapply(time_periods, function(x) {
        as.POSIXct(x, tz = attr(df$Timestamp, "tzone"))
      })
    } else if (is.numeric(df$Timestamp)) {
      time_periods <- lapply(time_periods, as.numeric)
    } else if  (inherits(df$Timestamp, "Date")) {
      time_periods <- lapply(time_periods, as.Date)
    }
  }
  
  # Ensure v_intercept is converted to the same data type as Timestamp
  if (!is.null(v_intercept)) {
    if (inherits(df$Timestamp, "POSIXct")) {
      v_intercept <- as.POSIXct(v_intercept, tz = attr(df$Timestamp, "tzone"))
    } else if (is.numeric(df$Timestamp)) {
      v_intercept <- as.numeric(v_intercept)
    } else if (inherits(df$Timestamp, "Date")) {
      v_intercept <- as.Date(v_intercept)
    } else {
      stop("Unsupported data type for Timestamp column.")
    }
  }
  
  # Set default values for sample_size and line_plot if they are NULL
  if (is.null(sample_size)) sample_size <- 0.2
  if (is.null(line_plot)) line_plot <- FALSE
  
  # Calculate the number of rows to sample and sample the data based on the specified sample_size
  sampling_percent <- round(sample_size * nrow(df))
  sampled_data <- df[(nrow(df) - sampling_percent + 1):nrow(df), ]
  
  # Group and count frequencies of cell IDs, then arrange by timestamp
  sampled_data <- sampled_data %>%
    dplyr::group_by(Cell.ID) %>%
    dplyr::mutate(Frequency = n()) %>%
    dplyr::arrange(Timestamp)
  
  axis_settings <- list(
    xaxis = list(
      title = "Timestamp",
      range = range(sampled_data$Timestamp)
    ),
    yaxis = list(
      title = "Cell ID",
      range = c(1, max(sampled_data$Cell.ID) + 2),
      tickmode = "linear",
      dtick = 10,
      tick0 = 0    )
  )
  
  # Create base plot with heatmap
  create_base_plot <- function(data, show_lines = FALSE) {
    p <- data %>%
      plotly::plot_ly(
        x = ~Timestamp, 
        y = ~Cell.ID, 
        z = ~Frequency,
        type = "heatmap", 
        hoverinfo = "text",  
        hovertext = ~sprintf(
          "Timestamp: %s<br>Cell ID: %d<br>Frequency: %d",
          Timestamp, Cell.ID, Frequency),
        showlegend = FALSE
      ) %>%
      plotly::colorbar(
        title = "Frequency",
        len = 0.5,
        thickness = 40,
        y = 0.8,
        yanchor = "middle"
      )
    
    # Add layout first
    p <- p %>%
      plotly::layout(
        autosize = TRUE,
        title = list(
          text = "Time series Flowmap",
          x = 0.03,
          y = 0.99,
          xanchor = "left"
        ),
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        showlegend = FALSE,
        hovermode = "closest" )
    
    # Add time period highlighting if specified
    if (!is.null(time_periods)) {
      # Create a list of shapes for all time periods
      shapes_list <- lapply(time_periods, function(period) {
        list(
          type = "rect",
          x0 = period[1],
          x1 = period[2],
          y0 = axis_settings$yaxis$range[1],
          y1 = axis_settings$yaxis$range[2],
          fillcolor = "black",
          opacity = 0.2,
          line = list(width = 0),
          layer = "below"
        )
      })
      
      # Add all shapes at once
      p <- p %>%
        plotly::layout(shapes = shapes_list)
    }
    
    # Add vertical line if specified
    if (!is.null(v_intercept)) {
      vline_df <- data.frame(
        x = c(v_intercept, v_intercept),
        y = c(min(data$Cell.ID), max(data$Cell.ID))
      )
      
      p <- p %>%
        plotly::add_trace(
          data = vline_df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(
            color = "black",
            width = 2,
            dash = "4px,4px"
          ),
          showlegend = FALSE,
          hoverinfo = "none"
        )
      
      p <- p %>%
        plotly::layout(
          autosize = TRUE,
          annotations = list(
            list(
              x = 1.02,
              y = 0.55,
              text = "  --- End of\nTraining data",
              showarrow = FALSE,
              font = list(
                color = "black",
                size = 14
              ),
              xref = "paper",
              yref = "paper",
              xanchor = "left",
              yanchor = "top",
              align = "left"
            )
          )
        )
    }
    
    # Add state transition lines if requested
    if (show_lines) {
      state_transitions <- data %>%
        dplyr::select(Timestamp, Cell.ID, Frequency) %>%
        dplyr::mutate(Next_State = lead(Cell.ID))
      
      p <- p %>%
        plotly::add_trace(
          data = state_transitions,
          x = ~Timestamp,
          y = ~Cell.ID,
          type = "scatter",
          mode = "markers",
          line = list(color = "gray", width = 1),
          marker = list(color = "transparent", size = 1),
          showlegend = FALSE
        )
    }
    
    return(p)
  }
  
  # Return the appropriate plot
  if (sample_size <= 1) {
    if (line_plot == TRUE) {
      return(create_base_plot(sampled_data, show_lines = TRUE))
    } else if (line_plot == FALSE) {
      return(create_base_plot(sampled_data, show_lines = FALSE))
    } else {
      stop("Invalid line_plot parameter. Use TRUE or FALSE.")
    }
  } else {
    stop("Invalid sample_size parameter. Use values between 0.1 to 1.")
  }
}
