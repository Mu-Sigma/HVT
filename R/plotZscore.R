#' @name plotZscore
#' @title Plots of z scores
#' @description This is the main function to plot the z scores against cell ids.
#' @param data Data frame. A data frame of cell id and features. 
#' @param cell_range Vector. A numeric vector of cell id range for which the plot should be displayed. 
#' Default is NULL, which plots all the cells.
#' @param segment_size Integer.  A numeric value to indicate the size of the bars in the plot. 
#' Default is 2.
#' @param reference_lines Vector. A numeric vector of confidence interval values for the 
#' reference lines in the plot. Default is c(-1.65, 1.65).  
#' @return A grid of plots of z score against cell id of teh given features.
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords EDA
#' @examples 
#'data("EuStockMarkets")
#'dataset <- data.frame(t = as.numeric(time(EuStockMarkets)),
#'                      DAX = EuStockMarkets[, "DAX"],
#'                      SMI = EuStockMarkets[, "SMI"],
#'                      CAC = EuStockMarkets[, "CAC"],
#'                      FTSE = EuStockMarkets[, "FTSE"])
#'rownames(EuStockMarkets) <- dataset$t
#'hvt.results<- trainHVT(dataset[-1],n_cells = 60, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE,quant_method = "kmeans")
#'col_names <- c("Cell.ID","DAX","SMI","CAC","FTSE")
#'data <- dplyr::arrange(dplyr::select(hvt.results[[3]][["summary"]],col_names),Cell.ID)
#'data <- round(data, 2)
#'plotZscore(data)
#' @export plotZscore


plotZscore <- function(data, cell_range = NULL, segment_size = 2,
                       reference_lines = c(-1.65, 1.65)) {
  
  ##for cran warning
  Value<-threshold_status <- NULL
  
  default_color = "gray50" 
  positive_color = "green3"
  negative_color = "red3"
  
  
  if (!"Cell.ID" %in% names(data)) {
    stop("Data must contain a 'Cell.ID' column")
  }
  

  # Function to round to nearest 0.2
  round_to_0.2 <- function(x) {
    round(x / 0.2) * 0.2
  }
  
  data_long <- data %>%
    pivot_longer(cols = !Cell.ID, 
                 names_to = "Variable", 
                 values_to = "Value") %>%
    mutate(
      threshold_status = case_when(
        Value >= 1.65 ~ "positive",
        Value <= -1.65 ~ "negative",
        TRUE ~ "within"
      ))
  
  if (!is.null(cell_range)) {
    data_long <- data_long %>%
      filter(Cell.ID >= cell_range[1], Cell.ID <= cell_range[2])
  }
  
  # Get all unique Cell IDs
  all_cell_ids <- sort(unique(data_long$Cell.ID))
  
  # Split data by Variable
  plot_data <- split(data_long, data_long$Variable)
  
  # Create plotting function
  create_plot <- function(df) {
    var_name <- unique(df$Variable)
    ggplot(df) +
      geom_vline(xintercept = reference_lines,
                 color = "black", linetype = "dashed", alpha = 0.5) +
      geom_segment(aes(x = 0, xend = Value, y = Cell.ID, yend = Cell.ID,
                       color = threshold_status),
                   size = segment_size) +
      scale_color_manual(
        values = c(
          "positive" = positive_color,
          "negative" = negative_color,
          "within" = default_color
        ),
        guide = "none") +
      scale_y_continuous(breaks = all_cell_ids,
                         labels = all_cell_ids,
                         expand = expansion(add = 1)) +
      labs(x = "Z-Score", y = "Cell ID", title = var_name) +
      scale_x_continuous(breaks = function(x) {
        unique(sort(c(x[1], -1.65, 0, 1.65, x[2])))
      }, labels = function(x) sprintf("%.2f", x)) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)),
        plot.margin = margin(t = 5, r = 20, b = 20, l = 20),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.line = element_line(color = "black"))
  }
  
  # Create all plots using purrr::map
  plots <- purrr::map(plot_data, create_plot)
  
  # Arrange plots in a grid with 3 columns
  arranged_plots <- do.call(gridExtra::grid.arrange, c(plots, ncol = 3))
  
  return(arranged_plots)
}