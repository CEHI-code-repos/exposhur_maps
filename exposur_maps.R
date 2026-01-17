library(sf)
library(tidyverse)
library(tigris)
library(units)

tribble(
  ~name      , ~year , ~st_abbrs                                                                             ,
  "ALLISON"  ,  2001 , c("MS", "PA", "FL", "LA", "TX")                                                       ,
  "IVAN"     ,  2004 , c("NC", "GA", "FL", "MS", "LA", "AL", "PA", "NY")                                     ,
  "CHARLEY"  ,  2004 , c("FL", "SC")                                                                         ,
  "FRANCES"  ,  2004 , c("SC", "GA", "PA", "NC", "FL")                                                       ,
  "KATRINA"  ,  2005 , c("AL", "LA", "MS", "FL")                                                             ,
  "RITA"     ,  2005 , c("LA", "TX")                                                                         ,
  "WILMA"    ,  2005 , c("FL")                                                                               ,
  "IKE"      ,  2008 , c("AR", "KY", "AL", "TX", "LA")                                                       ,
  "IRENE"    ,  2011 , c("DE", "DC", "MD", "ME", "MA", "RI", "NH", "PA", "VA", "CT", "VT", "NY", "NC", "NJ") ,
  "SANDY"    ,  2012 , c("PA", "MA", "DC", "NH", "WV", "VA", "MD", "DE", "CT", "NJ", "NY", "RI")             ,
  "MATTHEW"  ,  2016 , c("VA", "SC", "NC", "FL", "GA")                                                       ,
  "HARVEY"   ,  2017 , c("TX", "LA")                                                                         ,
  "IRMA"     ,  2017 , c("SC", "GA", "FL")                                                                   ,
  "FLORENCE" ,  2018 , c("VA", "SC", "NC")                                                                   ,
  "MICHAEL"  ,  2018 , c("NC", "VA", "AL", "GA", "FL")
) -> storm_data

source("get_hurdata.R")

al_hurdat <- get_hurdata("AL")
al_hurdat |>
  filter(map2_lgl(name, datetime, \(strm_name, strm_datetime) {
    any(strm_name == storm_data$name & year(strm_datetime) == storm_data$year)
  })) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  nest(.key = "track_points", .by = name) |>
  full_join(x = storm_data) -> storm_data

pmap(storm_data, \(track_points, ...) {
  track_points |>
    summarize(do_union = FALSE) |>
    st_cast("LINESTRING")
}) -> storm_data$hurr_track

pmap(storm_data, \(hurr_track, track_points, ...) {
  hurr_track |>
    st_segmentize(dfMaxLength = set_units(100, "km")) |>
    st_cast("POINT") |>
    bind_rows(track_points)
}) -> storm_data$label_points

pmap(storm_data, \(st_abbrs, year, ...) {
  census_year <- ifelse(year < 2012, 2000, 2010)
  st_abbr_to_fips <- tigris::fips_codes |>
    distinct(state, state_code) |>
    deframe()
  states(cb = TRUE, year = census_year) |>
    filter(STATE %in% st_abbr_to_fips[st_abbrs])
}) -> storm_data$state_sf

pmap(storm_data, \(st_abbrs, year, ...) {
  census_year <- ifelse(year < 2012, 2000, 2010)
  counties(state = st_abbrs, cb = TRUE, year = census_year)
}) -> storm_data$county_sf

pmap(storm_data, \(st_abbrs, year, ...) {
  census_year <- ifelse(year < 2012, 2000, 2010)
  map(st_abbrs, \(st_abbr) {
    tracts(state = st_abbr, cb = TRUE, year = census_year)
  }) |>
    bind_rows() |>
    mutate(GEOID = str_c(STATE, COUNTY, TRACT))
}) -> storm_data$tracts_sf

pmap(storm_data, \(name, year, ...) {
  path <- Sys.glob(str_glue("data/{name}-{year}/tract*_summary_wind.rds"))
  readRDS(path)
}) -> storm_data$summary_wind

pmap(storm_data, \(name, year, summary_wind, ...) {
  path <- Sys.glob(str_glue("data/{name}-{year}/tract*_long_prcp.rds"))
  long_prcp <- readRDS(path)

  long_prcp |>
    left_join(
      select(summary_wind, geoid, dist_from_storm_min_datetime),
      join_by(geoid)
    ) |>
    filter(
      date %within%
        interval(
          start = as.Date(dist_from_storm_min_datetime) - days(4),
          end = as.Date(dist_from_storm_min_datetime) + days(4)
        )
    ) |>
    summarise(total_prcp = sum(prcp_mean, na.rm = TRUE), .by = geoid)
}) -> storm_data$summary_prcp

convert_units <- function(value, from, to) {
  set_units(value, from, mode = "standard") |>
    set_units(
      to,
      mode = "standard"
    ) |>
    as.numeric()
}

# I modified the ggspatial scale just to make it have a single unit
source("ggspatial_scale.R")

make_plots <- function(measure, storms) {
  pmap(
    storm_data[storms, ],
    \(
      name,
      year,
      state_sf,
      county_sf,
      tracts_sf,
      summary_prcp,
      summary_wind,
      hurr_track,
      track_points,
      label_points,
      ...
    ) {
      wind_threshold <- 482.9878441
      prcp_threshold <- convert_units(6.296231, "in", "mm")
      if (measure == "wind") {
        relevant_data <- summary_wind |>
          mutate(
            plot_measure = as.numeric(vmax_sust_above_34kt_dur) > wind_threshold
          )
        color <- "#ffeda0"
      } else if (measure == "prcp") {
        relevant_data <- summary_prcp |>
          mutate(plot_measure = as.numeric(total_prcp) > prcp_threshold)
        color <- "#79abe1"
      } else {
        relevant_data <- summary_prcp |>
          left_join(summary_wind, join_by(geoid)) |>
          mutate(
            plot_measure = as.numeric(total_prcp) > prcp_threshold &
              as.numeric(vmax_sust_above_34kt_dur) > wind_threshold
          )
        color <- "#2ca25f"
      }

      tract_data <- tracts_sf |>
        left_join(relevant_data, join_by(GEOID == geoid)) |>
        st_filter(
          hurr_track,
          .predicate = st_is_within_distance,
          dist = as_units(250, "mi")
        ) |>
        st_simplify(dTolerance = 75)
      county_boundaries <- county_sf |>
        st_filter(tract_data)
      track_points <- track_points |>
        filter(hour(datetime) == 0)
      map_bbox <- st_bbox(county_boundaries)
      desired_asp <- 2.2
      map_width <- map_bbox[["xmax"]] - map_bbox[["xmin"]]
      map_height <- map_bbox[["ymax"]] - map_bbox[["ymin"]]
      map_aspect_ratio <- map_width / map_height
      if (map_aspect_ratio > desired_asp) {
        y_center <- mean(c(map_bbox[["ymax"]], map_bbox[["ymin"]]))
        new_height <- map_width * (1 / desired_asp)
        map_bbox[["ymax"]] <- y_center + (new_height / 2)
        map_bbox[["ymin"]] <- y_center - (new_height / 2)
      } else {
        x_center <- mean(c(map_bbox[["xmax"]], map_bbox[["xmin"]]))
        new_width <- map_height * desired_asp
        map_bbox[["xmax"]] <- x_center + (new_width / 2)
        map_bbox[["xmin"]] <- x_center - (new_width / 2)
      }
      map_bbox[["ymin"]] <- map_bbox[["ymin"]] -
        (0.1 * (map_bbox[["ymax"]] - map_bbox[["ymin"]]))

      map <- ggplot() +
        geom_sf(
          data = tract_data,
          aes(
            fill = .data[["plot_measure"]],
            color = after_scale(fill)
          ),
          lwd = 0.5
        ) +
        scale_fill_manual(
          values = c("TRUE" = color, "FALSE" = "white"),
          guide = "none"
        ) +
        geom_sf(
          data = hurr_track,
          color = "#924f4d",
          lwd = 1
        ) +
        geom_sf(
          data = state_sf,
          color = "black",
          fill = NA
        ) +
        geom_sf(
          data = track_points,
          fill = "white",
          color = "#924f4d",
          shape = 21
        ) +
        annotate(
          "rect",
          xmin = map_bbox[["xmin"]],
          xmax = map_bbox[["xmax"]],
          ymin = map_bbox[["ymin"]],
          ymax = map_bbox[["ymin"]] +
            0.1 * (map_bbox[["ymax"]] - map_bbox[["ymin"]]),
          fill = "white",
          color = "white"
        ) +
        annotation_scale(bar_cols = c("white", "white"), width_hint = 0.15) +
        coord_sf(
          xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
          ylim = c(map_bbox["ymin"], map_bbox["ymax"]),
          expand = FALSE
        ) +
        labs(title = str_glue("{name}-{year}")) +
        theme_void() +
        theme(
          plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
          title = element_text(face = "bold")
        )
    }
  )
}

pwalk(
  crossing(measure = c("wind", "prcp", "both"), storm_nums = list(1:8, 9:15)),
  \(measure, storm_nums) {
    plots <- make_plots(measure, storm_nums)

    while (length(plots) < 8) {
      plots[[length(plots) + 1]] <- patchwork::plot_spacer()
    }

    patchwork::wrap_plots(
      plots,
      ncol = 2,
      nrow = 4,
      widths = 1,
      heights = 1
    ) |>
      ggsave(
        filename = str_c(
          measure,
          "_",
          storm_nums[1],
          "-",
          storm_nums[length(storm_nums)],
          ".pdf"
        ),
        width = 8.5,
        height = 11,
        device = cairo_pdf
      )
  }
)

walk(
  c("wind", "prcp", "both"),
  \(measure) {
    make_plots(measure, 1:15) |>
      iwalk(\(plot, idx) {
        ggsave(
          plot,
          filename = str_c(
            measure,
            "_",
            str_to_lower(storm_data$name[idx]),
            ".png"
          ),
          width = 3.3 * 2,
          height = 2.1 * 2,
          dpi = 320,
          bg = "white"
        )
      })
  }
)
