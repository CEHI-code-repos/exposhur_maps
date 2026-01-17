read_hurdata <- function(file) {
  track_data <- readr::read_fwf(
    file,
    readr::fwf_cols(
      datetime = c(1, 14),
      record_id = 17,
      status = c(20, 21),
      latitude = c(24, 27),
      lat_hemisphere = 28,
      longitude = c(31, 35),
      lon_hemisphere = 36,
      max_sust_wind = c(39, 41),
      min_pressure = c(44, 47),
      wind_radii_34_kt_max_ext_ne = c(50, 53),
      wind_radii_34_kt_max_ext_se = c(56, 59),
      wind_radii_34_kt_max_ext_sw = c(62, 65),
      wind_radii_34_kt_max_ext_nw = c(68, 71),
      wind_radii_50_kt_max_ext_ne = c(74, 77),
      wind_radii_50_kt_max_ext_se = c(80, 83),
      wind_radii_50_kt_max_ext_sw = c(86, 89),
      wind_radii_50_kt_max_ext_nw = c(92, 95),
      wind_radii_64_kt_max_ext_ne = c(98, 101),
      wind_radii_64_kt_max_ext_se = c(104, 107),
      wind_radii_64_kt_max_ext_sw = c(110, 113),
      wind_radii_64_kt_max_ext_nw = c(116, 119),
      wind_radii_max = c(122, 125)
    ),
    readr::cols(
      datetime = readr::col_datetime(format = "%Y%m%d, %H%M"),
      record_id = readr::col_character(),
      status = readr::col_character(),
      latitude = readr::col_double(),
      lat_hemisphere = readr::col_character(),
      longitude = readr::col_double(),
      lon_hemisphere = readr::col_character(),
      max_sust_wind = readr::col_double(),
      min_pressure = readr::col_double(),
      wind_radii_34_kt_max_ext_ne = readr::col_double(),
      wind_radii_34_kt_max_ext_se = readr::col_double(),
      wind_radii_34_kt_max_ext_sw = readr::col_double(),
      wind_radii_34_kt_max_ext_nw = readr::col_double(),
      wind_radii_50_kt_max_ext_ne = readr::col_double(),
      wind_radii_50_kt_max_ext_se = readr::col_double(),
      wind_radii_50_kt_max_ext_sw = readr::col_double(),
      wind_radii_50_kt_max_ext_nw = readr::col_double(),
      wind_radii_64_kt_max_ext_ne = readr::col_double(),
      wind_radii_64_kt_max_ext_se = readr::col_double(),
      wind_radii_64_kt_max_ext_sw = readr::col_double(),
      wind_radii_64_kt_max_ext_nw = readr::col_double(),
      wind_radii_max = readr::col_double()
    )
  ) |>
    suppressWarnings()
  track_data <- track_data[!is.na(track_data$datetime), ]
  track_data$latitude <- track_data$latitude *
    ifelse(track_data$lat_hemisphere == "N", 1, -1)
  track_data$longitude <- track_data$longitude *
    ifelse(track_data$lon_hemisphere == "E", 1, -1)
  track_data <- track_data[
    -which(grepl("hemisphere$", colnames(track_data)))
  ]
  cols_neg_999 <- grepl("^(wind_radii|min_pressure)", colnames(track_data))
  track_data[cols_neg_999] <- lapply(track_data[cols_neg_999], \(vals) {
    ifelse(vals == -999, NA_real_, vals)
  })
  track_data$max_sust_wind <- ifelse(
    track_data$max_sust_wind == -999,
    NA_real_,
    track_data$max_sust_wind
  )

  hurr_data <- readr::read_fwf(
    file,
    readr::fwf_cols(
      basin = c(1, 2),
      atfc_cyclone_num = c(3, 4),
      name = c(19, 28),
      n_entries = c(34, 36)
    ),
    readr::cols(
      basin = readr::col_character(),
      atfc_cyclone_num = readr::col_integer(),
      year = readr::col_integer(),
      names = readr::col_character(),
      n_entries = readr::col_integer()
    )
  ) |>
    suppressWarnings()
  hurr_data <- hurr_data[!is.na(hurr_data$n_entries), ]
  hurr_data <- hurr_data[rep(seq_len(nrow(hurr_data)), hurr_data$n_entries), -4]

  cbind(hurr_data, track_data)
}

get_hurdata <- function(basin) {
  if (is.character(basin)) {
    basin <- toupper(basin)
  }

  hurdat_url <- if (identical(basin, "AL")) {
    "http://www.aoml.noaa.gov/hrd/hurdat/hurdat2.html"
  } else if (identical(basin, "EP")) {
    "http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-nepac.html"
  } else {
    stop("`basin` must be 'AL' or 'EP'.")
  }

  rvest::read_html(hurdat_url) |>
    rvest::html_text() |>
    read_hurdata() |>
    tibble::tibble()
}
