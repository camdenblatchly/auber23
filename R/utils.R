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


#' Gets tract-level broadband dataset
get_bb_tr <- function(geo = FALSE, st = "33") {
  
  bb_tr <- readr::read_csv(here::here("data/bb_tr_22.csv")) |>
    mutate(
      pct_100_20_loc = cnt_100_20 / cnt_total_locations,
      pct_25_3_loc = cnt_25_3 / cnt_total_locations,
      pct_fiber_loc = cnt_fiber_locations / cnt_total_locations
    )
  
  if (geo == FALSE) {
    return(bb_tr)
  }
  
  tracts <- tigris::tracts(state = st, cb = TRUE, year = 2021) %>%
    dplyr::filter(STATEFP <= "56") %>%
    tigris::shift_geometry(position = "below")
  
  bb_tr <- dplyr::left_join(
    bb_tr |> dplyr::filter(geoid_st == st),
    tracts,
    by = c("geoid_tr" = "GEOID")
  )
  
  return(bb_tr)
  
}

#' Formats Cut labels into percents
formatCutLabels <- function(breaks) {
  
  labels <- c()
  for (i in 2:length(breaks)) {
    new_entry <- paste0(
      "(", 
      scales::percent(breaks[i-1], accuracy = .1), 
      ", ", 
      scales::percent(breaks[i], accuracy = .1), 
      "]"
    )
    labels <- c(labels, new_entry)
  }
  
  return(labels)
}


get_choropleth <- function(dta, breaks, chrt_labels, outline_width = 0.1, value_col = "pct_25_3_loc") {
  
  fig <- dta %>%
    mutate(
      fill_var = cut(
        !!as.name(value_col),
        breaks$brks,
        labels = formatCutLabels(breaks$brks),
        include.lowest = TRUE
      )
    ) %>%
    sf::st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = fill_var), color = "#d0d2ce", linewidth = outline_width) +
    scale_fill_brewer(na.value = "#d0d2ce") + 
    # scale_color_brewer(na.value = "#d0d2ce") +
    theme_cori_map_presentation()
  
  fig <- fig + do.call(ggplot2::labs, chrt_labels)

  return(fig)
  
}