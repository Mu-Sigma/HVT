#' @name summary
#' @title Table for displaying summary
#' @description This is the main function for displaying summary from model training and scoring
#' @param data List. A listed object from trainHVT or scoreHVT
#' @param limit Numeric. A value to indicate how many rows to display.
#' Default value is 20.
#' @return A consolidated table of summary from trainHVT and scoreHVT.
#' @author Vishwavani <vishwavani@@mu-sigma.com>, Alimpan Dey <alimpan.dey@@mu-sigma.com>
#' @importFrom dplyr mutate 
#' @keywords Data Analysis
#' @examples
#' data <- datasets::EuStockMarkets
#' dataset <- as.data.frame(data)
#' #model training
#' hvt.results <- trainHVT(dataset, n_cells = 60, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE, quant_method = "kmeans", dim_reduction_method = 'sammon')
#' summary(data =  hvt.results)
#' @export summary

summary <- function(data, limit = 20, scroll = TRUE) {
  
  if (class(data) == "hvt.object") {
    
    # Function to calculate scroll height based on the number of rows
    scrolLimit <- function(noOfRows) {
      if (noOfRows < 10) {
        swe <- paste(as.character(noOfRows * 50), "px")
      } else {
        swe <- "400px"
      }
      return(swe)
    }
    
    # Limit the number of rows displayed for each table
    apply_limit <- function(table, limit) {
      if (is.null(limit)) {
        limit <- if (nrow(table) > 20) 20 else nrow(table)
      }
      head(table, limit)
    }
    
    if (length(data) >= 3 && "compression_summary" %in% names(data[[3]])) {
      compression_summary_table <- data[[3]]$compression_summary %>% as.data.frame()
      compression_summary_table <- apply_limit(compression_summary_table, limit)
      
      table_1 <- compression_summary_table %>%
        dplyr::mutate(percentOfCellsBelowQuantizationErrorThreshold = round(compression_summary_table$percentOfCellsBelowQuantizationErrorThreshold, 2)) %>%
        dplyr::mutate(percentOfCellsBelowQuantizationErrorThreshold = dplyr::case_when(
          percentOfCellsBelowQuantizationErrorThreshold >= 0.8 ~ 
            kableExtra::cell_spec(percentOfCellsBelowQuantizationErrorThreshold, "html", color = "green"),
          TRUE ~ 
            kableExtra::cell_spec(percentOfCellsBelowQuantizationErrorThreshold, "html", color = "black")
        ))
      
      table_1 <- knitr::kable(table_1, "html", escape = FALSE, align = "c") %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 10.5) 
       # kableExtra::scroll_box(width = "90%", height = scrolLimit(nrow(compression_summary_table)))
      
      return(table_1)
      
    } else if ("scoredPredictedData" %in% names(data)) {
      scorehvt_table <- data$scoredPredictedData %>% as.data.frame()
      scorehvt_table <- apply_limit(scorehvt_table, limit)
      
      value <- data[["model_info"]][["scored_model_summary"]][["mad.threshold"]]
      
      table_2 <- scorehvt_table %>%
        dplyr::mutate(Quant.Error = dplyr::case_when(
          scorehvt_table$Quant.Error > value ~ 
            kableExtra::cell_spec(Quant.Error, "html", color = "red"),
          TRUE ~ 
            kableExtra::cell_spec(Quant.Error, "html", color = "black")
        ))
      
      table_2 <- knitr::kable(table_2, "html", escape = FALSE, align = "c") %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>%
        kableExtra::scroll_box(width = "100%", height = scrolLimit(nrow(scorehvt_table)))
      
      return(table_2)
      
    } else if ("actual_predictedTable" %in% names(data)) {
      scoreLayeredhvt_table <- data$actual_predictedTable %>% as.data.frame()
      scoreLayeredhvt_table <- apply_limit(scoreLayeredhvt_table, limit)
      
      table_3 <- knitr::kable(scoreLayeredhvt_table, "html", escape = FALSE, align = "c") %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>%
        kableExtra::scroll_box(width = "100%", height = scrolLimit(nrow(scoreLayeredhvt_table)))
      
      return(table_3)
    }
    
  } else {
    base::summary(data)
  }
}
