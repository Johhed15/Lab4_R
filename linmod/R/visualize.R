#' @title dplyr plot
#' 
#' @name visualize_airport_delays
#' 
#' 
#' @description 
#' A function which uses dplyr on the airports dataset from the nycflights13 package 
#' and manipulates it and returns a plot.
#' 
#' 
#' @export visualize_airport_delays
#' 
#' @import ggplot2 tidyr nycflights13 dplyr 
#' 
#' @importFrom dplyr %>% 


visualize_airport_delays <- function(){
  ap <- dplyr::as_tibble(nycflights13::airports)
  f <- dplyr::as_tibble(nycflights13::flights)
  
  f <- f %>%  na.omit |> dplyr::select(arr_delay,dest) |> dplyr::group_by(dest) |> dplyr::summarise(avg = mean(arr_delay))
  ap <- ap |> dplyr::filter(ap$faa %in% f$dest)
  f <- f |> dplyr::filter(f$dest %in% ap$faa)
  names(f)[names(f) == "dest"] <- "faa"
  
  df <- dplyr::inner_join(f, ap, "faa")
  df <- df |> dplyr::select(faa,avg,lon,lat)
  
  p1 <- ggplot2::ggplot(df, aes(x = lon, y = lat, color = avg)) + ggplot2::geom_point(size = 2) + ggplot2::theme_bw() + ggplot2::scale_color_viridis_c()
  p1 <- p1 + ggplot2::labs(title = "Scatterplot of flight delays in USA", x = "Longitude", y = "Latitude", color = "Avg. delay")
  p1
}
