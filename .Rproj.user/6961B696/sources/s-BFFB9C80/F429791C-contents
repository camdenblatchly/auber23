#' Gets county-level broadband data-set
get_bb_co <- function(geo = FALSE) {
  
  bb_co <- readr::read_csv(here::here("data/bb_co_22.csv")) %>%
    mutate(
      pct_100_20_loc = cnt_100_20_loc / cnt_total_locations,
      pct_25_3_loc = cnt_25_3_loc / cnt_total_locations,
      pct_fiber_loc = cnt_fiber_locations / cnt_total_locations
    )
  
  if (geo == FALSE) {
    return(bb_co)
  }
  
  counties <- tigris::counties(cb = TRUE, year = 2021) %>%
    dplyr::filter(STATEFP <= "56") %>%
    tigris::shift_geometry(position = "below")
  
  bb_co <- dplyr::left_join(
    bb_co,
    counties,
    by = c("geoid_co" = "GEOID")
  )
  
  return(bb_co)
  
}


#' Gets tract-level broadband data-set
get_bb_tr <- function() {
  
  bb_tr <- readr::read_csv("data/bb_tr_22.csv")
  
  return(bb_tr)
  
}

#' Formats Cut labels into percents
formatCutLabels <- function(breaks) {
  
  labels <- c()
  for (i in 2:length(breaks)) {
    new_entry <- paste0("(", scales::percent(breaks[i-1]), ", ", scales::percent(breaks[i]), "]")
    labels <- c(labels, new_entry)
  }
  
  return(labels)
}


get_choropleth <- function(dta, breaks, chrt_labels) {
  
  fig <- dta %>%
    mutate(
      fill_var = cut(
        pct_25_3_loc,
        breaks$brks,
        labels = formatCutLabels(breaks$brks)
      )
    ) %>%
    sf::st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = fill_var)) +
    scale_fill_cori(
      palette = "ctg3gn",
    ) +
    labs(chrt_labels) +
    theme_cori_map() +

  return(fig)
  
  # save_plot(fig, here::here("export/choropleth_jenks_co.png"), chart_height = 8)
  
  
}