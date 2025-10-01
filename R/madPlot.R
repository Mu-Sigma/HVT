#' @keywords internal


madPlot <- function(hvt.scoring) {
  requireNamespace("ggplot2")       #deldir function
  requireNamespace("scales")       #deldir function
  
  qe = hvt.scoring[["predictInput"]][["quant.err"]]
  df_scored = hvt.scoring[["scoredPredictedData"]]
  qe_val = df_scored$Quant.Error
  
  
  if (qe < 0.75) {
    x_breaks = seq(qe, 0.75, length.out = 10) %>% round(2)
    MAD_list = lapply(seq(qe, 0.75, by = 0.01), function(x) {
      anom_points = sum(qe_val > x)
      max_cells = length(qe_val)
      perct_anom = (anom_points / max_cells)
      
      return(c(x, anom_points, max_cells, perct_anom))
    })
  } else if (qe > 0.75) {
    x_breaks = seq(qe, (qe + 0.75), length.out = 10) %>% round(2)
    MAD_list = lapply(seq(qe, (qe + 0.75), by = 0.01), function(x) {
      anom_points = sum(qe_val > x)
      max_cells = length(qe_val)
      perct_anom = (anom_points / max_cells)
      
      return(c(x, anom_points, max_cells, perct_anom))
    })
    
    
    
  }
  
  
  comp_nclust_anom = MAD_list %>% purrr::reduce(rbind) %>% as.data.frame()
  names(comp_nclust_anom) = c(
    "Mean_Absolute_Deviation",
    "anom_points",
    "max_cells",
    "Percentage_of_Anomalous_Points"
  )
  
  ############################################
  
  mark_x = which(comp_nclust_anom$Percentage_of_Anomalous_Points < 0.01)
  mark_x = comp_nclust_anom[mark_x[1], ]
  
  
  
  mad_plot =
    ggplot()  +
    geom_point(
      data = comp_nclust_anom,
      mapping = aes(x = Mean_Absolute_Deviation,
                    y = Percentage_of_Anomalous_Points),
      shape = "circle",
      size = 1.5,
      colour = "midnightblue"
    ) +
    geom_hline(yintercept = 0.01,
               linetype = "dashed",
               colour = "brown2") +
    geom_vline(
      xintercept = mark_x$Mean_Absolute_Deviation,
      linetype = "dashed",
      colour = "brown2"
    ) +
  
    
    annotate(
      "text",
      x = mark_x$Mean_Absolute_Deviation + (0.03 * max(
        comp_nclust_anom$Mean_Absolute_Deviation
      )),
      y = mark_x$Percentage_of_Anomalous_Points + (0.1),
      label = mark_x$Mean_Absolute_Deviation,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = qe,
      y = 0.01 + (0.1),
      label = "1%",
      fontface = "bold"
    ) +
    
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12),
      axis.text = element_text(size = 8),
      # axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 8)
    ) +
    labs(title = "Mean Absolute Deviation Plot") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_x_continuous(breaks =x_breaks)
  
  return(mad_plot)
}
