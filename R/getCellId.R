#' @keywords internal

getCellId <-  function(hvt.results, seed = 123) {
  generic_col=c("Segment.Level","Segment.Parent","Segment.Child","n","Quant.Error")
  temp_summary=hvt.results[[3]][["summary"]] %>% dplyr::select(!generic_col) %>% dplyr::mutate(id=row_number())
  cent_val= temp_summary %>% subset(.,stats::complete.cases(.)) 
  set.seed(seed)
  sammon_1d_cord <- MASS::sammon(
    d = stats::dist(cent_val %>% dplyr::select(!id),method = "manhattan"),
    niter = 10 ^ 5,
    trace = FALSE,
    k=1
  )$points
  temp_df=data.frame(sammon_1d_cord,id=cent_val$id)%>%dplyr::arrange(sammon_1d_cord) %>% dplyr::mutate(Cell.ID=row_number()) %>% dplyr::select(!sammon_1d_cord)
  temp_summary = dplyr::left_join(temp_summary,temp_df,by="id") %>% select(!"id")
  hvt.results[[3]][["summary"]]$Cell.ID=temp_summary$Cell.ID

    return(hvt.results[[3]][["summary"]])
  }




