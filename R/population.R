#' Spatial Temporal Distribution Plot
#'
#' @param df a dataframe containing columns "x", "y", "date", and "id"
#' @param proj4 a character string indicating the proj.4 definition of the
#' coordinate reference system defining the relocations
#' @export
dist_map <- function(df, proj4) {
  mean_sf <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      meanX = mean(x, na.rm = T),
      meanY = mean(y, na.rm = T),
      meanyear = factor(round(mean(lubridate::year(date)), 0))
    ) %>%
    sf::st_as_sf(coords = c("meanX", "meanY"), remove = FALSE, crs = proj4)

  print(ggplot(data = mean_sf) +
    geom_sf(aes(fill = meanyear)) +
    ggrepel::geom_text_repel(mapping = aes(
      x = meanX, y = meanY,
      label = id, color = meanyear
    )) +
    ggtitle("General Spatial and Temporal Distribution of Individuals"))

  return(mean_sf)
}

#' Population Timeline Plot
#'
#'
#' @param df a dataframe containing columns "x", "y", "date", and "id"
#' @export
plot_timeline <- function(df) {
  df %>%
    group_by(id) %>%
    summarise(start_date = min(date), end_date = max(date)) %>%
    ggplot() + geom_segment(aes(
      x = start_date, xend = end_date,
      y = id, yend = id, color = id
    ),
    linetype = 1, size = 2
    )
}
