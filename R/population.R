#' Spatial Temporal Distribution Plot
#'
#' @param df a dataframe containing columns "x", "y", "date", and "id"
#' @param proj4 a character string indicating the proj.4 definition of the
#' coordinate reference system defining the relocations
#' @param labels logical, indicating whether or not to label points with IDs, implemented using
#' \link[ggrepel]{geom_text_repel}
#' @export
#' @importFrom ggplot2 ggplot aes geom_sf labs
dist_map <- function(df, proj4, labels = TRUE) {
  mean_sf <- df %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(
      meanX = mean(.data$x, na.rm = T),
      meanY = mean(.data$y, na.rm = T),
      meanyear = factor(round(mean(lubridate::year(.data$date)), 0))
    ) %>%
    sf::st_as_sf(coords = c("meanX", "meanY"), remove = FALSE, crs = proj4)

 p <- ggplot(data = mean_sf) +
    geom_sf(aes(color = .data$meanyear, fill = .data$meanyear)) +
    labs(title = "General Spatial and Temporal Distribution of Individuals",
    x = "Latitude",  y = "Longitude", color = "Year", fill = "Year")

 if(labels == TRUE) {
   print(p + ggrepel::geom_text_repel(mapping = aes(
     x = .data$meanX, y = .data$meanY,
     label = .data$id, color = .data$meanyear
   )))
 } else {print(p)}


  return(mean_sf)
}

#' Population Timeline Plot
#'
#'
#' @param df a dataframe containing columns "x", "y", "date", and "id"
#' @export
#' @importFrom ggplot2 geom_segment theme_classic theme element_text
plot_timeline <- function(df) {
  df %>%
    group_by(.data$id) %>%
    summarise(start_date = min(.data$date), end_date = max(.data$date)) %>%
    ggplot() + geom_segment(aes(
      x = .data$start_date, xend = .data$end_date,
      y = .data$id, yend = .data$id
    ),
    linetype = 1, size = 2
    ) +
    labs(title = "Timeline Plot of Tagged Individuals",
         x = "Date",  y = "ID", color = "ID") +
    theme_classic() +
    theme(legend.position = "none", plot.title = element_text(hjust = .5))
}
