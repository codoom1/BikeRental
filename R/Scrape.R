# Build the processed dataset used in the published bike-rental analysis.
#
# Run this script from the repository root. Before running it, download the
# monthly Capital Bikeshare trip-history CSV files for April 2020 through
# May 2023 and place them in data/raw/.
#
# Expected filename pattern:
#   YYYYMM-capitalbikeshare-tripdata.csv
#
# The script writes intermediate files to data/processed/ and the final
# analysis dataset to data/paperbike_data.csv.

required_packages <- c("dplyr", "lubridate", "purrr", "readr", "rvest", "tibble")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required R packages: ",
      paste(missing_packages, collapse = ", "),
      ".\nInstall them with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

start_date <- as.Date("2020-04-01")
end_date <- as.Date("2023-05-31")

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
output_path <- file.path("data", "paperbike_data.csv")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

month_starts <- seq(
  from = lubridate::floor_date(start_date, unit = "month"),
  to = lubridate::floor_date(end_date, unit = "month"),
  by = "month"
)

trip_paths <- file.path(
  raw_dir,
  paste0(
    format(month_starts, "%Y%m"),
    "-capitalbikeshare-tripdata.csv"
  )
)

missing_trip_files <- trip_paths[!file.exists(trip_paths)]

if (length(missing_trip_files) > 0) {
  stop(
    paste0(
      "Missing ", length(missing_trip_files), " Capital Bikeshare file(s) in ",
      raw_dir, ".\nFirst missing file: ", missing_trip_files[[1]], "\n",
      "Download the monthly files from ",
      "https://capitalbikeshare.com/system-data"
    ),
    call. = FALSE
  )
}

find_column <- function(data, candidates, label) {
  normalized_names <- tolower(gsub("[^a-z0-9]+", "_", names(data)))
  normalized_candidates <- tolower(gsub("[^a-z0-9]+", "_", candidates))
  match_index <- match(normalized_candidates, normalized_names, nomatch = 0L)
  match_index <- match_index[match_index > 0L]

  if (length(match_index) == 0L) {
    stop(
      "Could not find the ", label, " column. Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  names(data)[match_index[[1]]]
}

parse_trip_datetime <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c(
      "ymd HMS", "ymd HM", "mdy HMS", "mdy HM",
      "ymd IMS p", "ymd IM p", "mdy IMS p", "mdy IM p"
    ),
    quiet = TRUE
  )
}

read_monthly_trips <- function(path) {
  message("Reading ", basename(path))
  trips <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  ride_type_column <- find_column(
    trips,
    c("rideable_type", "bike_type", "biketype"),
    "bike type"
  )
  start_time_column <- find_column(
    trips,
    c("started_at", "start_date", "start_time"),
    "trip start time"
  )
  latitude_column <- find_column(
    trips,
    c("start_lat", "start_latitude", "latitude"),
    "start latitude"
  )
  longitude_column <- find_column(
    trips,
    c("start_lng", "start_lon", "start_longitude", "longitude"),
    "start longitude"
  )
  membership_column <- find_column(
    trips,
    c("member_casual", "member_type", "membership", "user_type"),
    "membership type"
  )

  tibble::tibble(
    bike_type = as.character(trips[[ride_type_column]]),
    date = as.Date(parse_trip_datetime(trips[[start_time_column]])),
    latitude = suppressWarnings(as.numeric(trips[[latitude_column]])),
    longitude = suppressWarnings(as.numeric(trips[[longitude_column]])),
    membership = tolower(trimws(as.character(trips[[membership_column]])))
  ) |>
    dplyr::filter(
      !is.na(date),
      date >= start_date,
      date <= end_date
    ) |>
    dplyr::mutate(
      membership = dplyr::case_when(
        membership %in% c("member", "registered", "subscriber") ~ "member",
        membership %in% c("casual", "customer") ~ "casual",
        TRUE ~ membership
      )
    )
}

aggregate_daily_trips <- function(trips) {
  unknown_membership <- setdiff(unique(trips$membership), c("member", "casual"))

  if (length(unknown_membership) > 0L) {
    warning(
      "Unrecognized membership values were excluded from rental counts: ",
      paste(unknown_membership, collapse = ", "),
      call. = FALSE
    )
  }

  trips |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      registered = sum(membership == "member", na.rm = TRUE),
      casual = sum(membership == "casual", na.rm = TRUE),
      total_rentals = registered + casual,
      latitude = mean(latitude, na.rm = TRUE),
      longitude = mean(longitude, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(date)
}

daily_mode <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) {
    return(NA_character_)
  }

  counts <- table(x)
  names(counts)[which.max(counts)]
}

extract_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]+", "", as.character(x))))
}

find_weather_column <- function(data, pattern, label) {
  match_index <- grep(pattern, names(data), ignore.case = TRUE)

  if (length(match_index) == 0L) {
    stop(
      "Could not find the ", label, " column in the Time and Date table.",
      call. = FALSE
    )
  }

  names(data)[match_index[[1]]]
}

scrape_daily_weather <- function(date) {
  message("Scraping weather for ", date)

  url <- paste0(
    "https://www.timeanddate.com/weather/usa/washington-dc/historic",
    "?month=", format(date, "%m"),
    "&year=", format(date, "%Y"),
    "&hd=", format(date, "%Y%m%d")
  )

  page <- rvest::read_html(url)
  weather_table <- rvest::html_element(page, "table#wt-his")

  if (inherits(weather_table, "xml_missing")) {
    tables <- rvest::html_elements(page, "table")
    if (length(tables) < 2L) {
      stop("No weather table found for ", date, ": ", url, call. = FALSE)
    }
    weather_table <- tables[[2]]
  }

  weather <- rvest::html_table(weather_table, fill = TRUE)

  temp_column <- find_weather_column(weather, "^Temp", "temperature")
  wind_column <- find_weather_column(weather, "^Wind", "wind")
  humidity_column <- find_weather_column(weather, "^Humidity", "humidity")
  barometer_column <- find_weather_column(weather, "^Barometer", "barometer")
  visibility_column <- find_weather_column(weather, "^Visibility", "visibility")
  conditions_column <- find_weather_column(
    weather,
    "Weather|Conditions",
    "weather conditions"
  )

  tibble::tibble(
    date = date,
    Temp = mean(extract_numeric(weather[[temp_column]]), na.rm = TRUE),
    Wind = mean(extract_numeric(weather[[wind_column]]), na.rm = TRUE),
    Humidity = mean(extract_numeric(weather[[humidity_column]]), na.rm = TRUE),
    Barometer = mean(extract_numeric(weather[[barometer_column]]), na.rm = TRUE),
    Visibility = mean(extract_numeric(weather[[visibility_column]]), na.rm = TRUE),
    Weather = daily_mode(as.character(weather[[conditions_column]]))
  )
}

district_holidays <- as.Date(c(
  "2020-01-01", "2020-01-20", "2020-02-17", "2020-04-08",
  "2020-05-25", "2020-06-19", "2020-07-04", "2020-09-07",
  "2020-10-12", "2020-11-11", "2020-11-26", "2020-12-25",
  "2021-01-01", "2021-01-18", "2021-02-15", "2021-04-16",
  "2021-05-31", "2021-06-18", "2021-07-04", "2021-09-06",
  "2021-10-11", "2021-11-11", "2021-11-25", "2021-12-25",
  "2022-01-01", "2022-01-17", "2022-02-21", "2022-04-15",
  "2022-05-30", "2022-06-19", "2022-07-04", "2022-09-05",
  "2022-10-10", "2022-11-11", "2022-11-24", "2022-12-25",
  "2023-01-01", "2023-01-02", "2023-01-16", "2023-02-20",
  "2023-04-17", "2023-05-29", "2023-06-19", "2023-07-04",
  "2023-09-04", "2023-10-09", "2023-11-10", "2023-11-23",
  "2023-12-25"
))

date_sequence <- seq(start_date, end_date, by = "day")

trip_data <- purrr::map_dfr(trip_paths, read_monthly_trips)
daily_bike_data <- aggregate_daily_trips(trip_data)

readr::write_csv(
  daily_bike_data,
  file.path(processed_dir, "bike_rental.csv")
)

weather_cache_path <- file.path(processed_dir, "weather_data.csv")

if (file.exists(weather_cache_path)) {
  message("Using cached weather data: ", weather_cache_path)
  daily_weather <- readr::read_csv(
    weather_cache_path,
    show_col_types = FALSE
  ) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= start_date, date <= end_date)
} else {
  daily_weather <- tibble::tibble(
    date = as.Date(character()),
    Temp = double(),
    Wind = double(),
    Humidity = double(),
    Barometer = double(),
    Visibility = double(),
    Weather = character()
  )
}

uncached_weather_dates <- setdiff(date_sequence, daily_weather$date)

if (length(uncached_weather_dates) > 0L) {
  message(
    "Collecting ", length(uncached_weather_dates),
    " missing day(s) of weather data."
  )
  new_weather <- purrr::map_dfr(uncached_weather_dates, scrape_daily_weather)
  daily_weather <- dplyr::bind_rows(daily_weather, new_weather) |>
    dplyr::distinct(date, .keep_all = TRUE) |>
    dplyr::arrange(date)
}

readr::write_csv(daily_weather, weather_cache_path)

missing_bike_dates <- setdiff(date_sequence, daily_bike_data$date)
missing_weather_dates <- setdiff(date_sequence, daily_weather$date)

if (length(missing_bike_dates) > 0L) {
  stop(
    "Bike data are missing ", length(missing_bike_dates),
    " expected date(s). First missing date: ", missing_bike_dates[[1]],
    call. = FALSE
  )
}

if (length(missing_weather_dates) > 0L) {
  stop(
    "Weather data are missing ", length(missing_weather_dates),
    " expected date(s). First missing date: ", missing_weather_dates[[1]],
    call. = FALSE
  )
}

paper_data <- daily_bike_data |>
  dplyr::inner_join(daily_weather, by = "date") |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    Year = lubridate::year(date),
    Month = lubridate::month(date),
    day = lubridate::day(date),
    Weekday = as.integer(!lubridate::wday(date, week_start = 1) %in% c(6, 7)),
    season = dplyr::case_when(
      Month %in% 3:5 ~ "Spring",
      Month %in% 6:8 ~ "Summer",
      Month %in% 9:11 ~ "Autumn",
      TRUE ~ "Winter"
    ),
    holiday = as.integer(date %in% district_holidays),
    workinday = as.integer(Weekday == 1L & holiday == 0L)
  ) |>
  dplyr::select(
    date, registered, casual, total_rentals, latitude, longitude,
    Temp, Wind, Humidity, Barometer, Visibility, Weather,
    Year, Month, day, Weekday, season, holiday, workinday
  ) |>
  dplyr::mutate(instant = dplyr::row_number(), .before = date)

if (nrow(paper_data) != length(date_sequence)) {
  stop(
    "Expected ", length(date_sequence), " daily records but produced ",
    nrow(paper_data), ".",
    call. = FALSE
  )
}

readr::write_csv(
  paper_data,
  file.path(processed_dir, "paperbike_data.csv")
)
readr::write_csv(paper_data, output_path)

message(
  "Wrote ", nrow(paper_data), " records from ",
  min(paper_data$date), " through ", max(paper_data$date),
  " to ", output_path
)
