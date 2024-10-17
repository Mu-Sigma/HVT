#' @name displayTable
#' @title Table for displaying summary
#' @description This is the main function for displaying summary from model training and scoring
#' @param data List. A listed object from trainHVT or scoreHVT
#' @param columnName Character. Name of the column that needs highlighting.
#' @param value Numeric. The value above will be highlighted in red or green.
#' @param tableType Character. Type of table to generate ('summary', 'compression' and 'metrics')
#' @param scroll Logical. A value to have a scroll or not in the table.
#' @param limit Numeric. A value to indicate how many rows to display.
#' Applicable for summary tableType.
#' @return A consolidated table of results from trainHVT and scoreHVT.
#' @author Vishwavani <vishwavani@@mu-sigma.com>, Alimpan Dey <alimpan.dey@@mu-sigma.com>
#' @seealso \code{\link{trainHVT}} 
#' @importFrom rlang sym
#' @importFrom dplyr mutate across where
#' @keywords EDA
#' @examples
#' data <- datasets::EuStockMarkets
#' dataset <- as.data.frame(data)
#' #model training
#' hvt.results <- trainHVT(dataset, n_cells = 60, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE, quant_method = "kmeans", dim_reduction_method = 'sammon')
#' displayTable(data =  hvt.results$model_info$distance_measures, tableType = "metrics")
#' displayTable(data = hvt.results[[3]]$compression_summary,
#' columnName = 'percentOfCellsBelowQuantizationErrorThreshold', 
#' value = 0.8, tableType = "compression")
#' displayTable(data =hvt.results[[3]][['summary']], columnName= 'Quant.Error',
#' value = 0.1, tableType = "summary", scroll = TRUE)
#' @export displayTable

displayTable <- function(data, value = 0.2,limit= NULL) {
  data <- data %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, 4)
  
  # Function to calculate scroll height based on the number of rows
  scrolLimit <- function(noOfRows) {
    if (noOfRows < 10) {
      swe <- paste(as.character(noOfRows * 50), "px")
    } else {
      swe <- "400px"
    }
    return(swe)
  }
  
  # Check if scrolling is needed (this variable is defined but not used)
  scroll <- nrow(data) > 10 || ncol(data) > 10
  
  # Limit the number of rows displayed
  if (is.null(limit)){
  limit <- if (nrow(data) > 100) 100 else nrow(data)
  data <- head(data, limit)
  }
  if ("L1_Metrics" %in% colnames(data)) {
    kable_table <- knitr::kable(data, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive", "bordered")) %>%
      kableExtra::collapse_rows(columns = 1, valign = "middle")
    return(kable_table)
    
  } else if ("percentOfCellsBelowQuantizationErrorThreshold" %in% colnames(data)) {
    table_1 <- data %>%
      dplyr::mutate(percentOfCellsBelowQuantizationErrorThreshold = round(percentOfCellsBelowQuantizationErrorThreshold, 2)) %>%
      dplyr::mutate(percentOfCellsBelowQuantizationErrorThreshold = dplyr::case_when(
        percentOfCellsBelowQuantizationErrorThreshold >= 0.8 ~ 
          kableExtra::cell_spec(percentOfCellsBelowQuantizationErrorThreshold, "html", color = "green"),
        TRUE ~ 
          kableExtra::cell_spec(percentOfCellsBelowQuantizationErrorThreshold, "html", color = "black")
      ))
    
    table_1 <- knitr::kable(table_1, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) 
    if (scroll){
      table_1 <- table_1 %>% kableExtra::scroll_box(width = "100%", height = scrolLimit(nrow(data)))
    }

    return(table_1)
    
  } else if ("Quant.Error" %in% colnames(data)) {
    table_2 <- data %>%
      dplyr::mutate(Quant.Error = dplyr::case_when(
        Quant.Error > value ~ 
          kableExtra::cell_spec(Quant.Error, "html", color = "red"),
        TRUE ~ 
          kableExtra::cell_spec(Quant.Error, "html", color = "black")
      ))
    
    table_2 <- knitr::kable(table_2, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) 
    if (scroll){
      table_2 <- table_2 %>% kableExtra::scroll_box(width = "100%", height = scrolLimit(nrow(data)))
    }
    return(table_2)
    
  } else {
    table_3 <- knitr::kable(data, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive")) 
     if (scroll){
       table_3 <- table_3 %>% kableExtra::scroll_box(width = "100%", height = scrolLimit(nrow(data)))
     }
    return(table_3)
  }
}
