#' @name displayTable
#' @title function for displaying table
#' @description This is the main function for displaying data in table format
#' @param data Data frame. The dataframe to be displayed in table format.
#' @param scroll Logical. A value to have a scroll or not in the table. 
#' Default is TRUE. 
#' @param limit Numeric. A value to indicate how many rows to display.
#' Default is 20.
#' @return A table with proper formatting for html notebook
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @importFrom dplyr mutate 
#' @keywords Table_Formatting
#' @examples
#' data <- datasets::EuStockMarkets
#' dataset <- as.data.frame(data)
#' displayTable(dataset)
#' @export displayTable

displayTable <- function(data, scroll = TRUE,limit= 20) {
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
    limit <- if (nrow(data) > 20) limit else nrow(data)
    data <- head(data, limit)
  
    table_3 <- knitr::kable(data, "html", escape = FALSE, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"),font_size = 10.5) 
    if (scroll){
      table_3 <- table_3 %>% kableExtra::scroll_box(width = "100%", height = scrolLimit(nrow(data)))
    }
    return(table_3)
  
}
