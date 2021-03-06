% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_station_data.R
\name{get_station_data}
\alias{get_station_data}
\title{Gets the station data for the station closest to the given longitude and latitude}
\usage{
get_station_data(ghncd_meta_data, lat_lon_df, search_country_code,
  record_len = 100, radius = 10)
}
\arguments{
\item{lat_lon_df}{Similar to the input for lat_lon_df from
\code{rnoaa::rnoaa::meteo_nearby_stations}. A single row dataframe that
contains the
latitude, longitude, and a unique identifier for each location (id). For
an example of the proper format for this dataframe, see the example
below. Latitude and longitude must both be in units of decimal degrees.
Southern latitudes and Western longitudes should be given as negative values.}

\item{search_country_code}{Two letter string representing the country/state
code used in GHCN Daily.
For Australia the code is "AS". For a full list of country codes refer to
the function \code{rnoaa::ghcnd_countries} or \code{rnoaa::ghncd_states}}

\item{record_len}{Minimum record length considered. To compare plots over
multiple years we scale the observations relative to mean climate.
We recommend using a station with a longer record length (Default is 100).}

\item{radius}{A numeric value giving the radius in kilometres to search
for the closest station to the given longitude and latitude. (Default is 10).
(Similar to the input for radius from
\code{rnoaa::meteo_nearby_stations})}

\item{ghcnd_meta_data}{Data frame that contains the station meta data
for the Global Historical Climatology Network Daily (GHCND). The most up
to date version of this meta data can be returned from function
\code{rnoaa::ghncd_stations}. However, we have also stored a copy
as a data object to save time.}
}
\value{
Returns a dataframe with columns id, date, and the relevant
climate variables for precipitation and temperature.
}
\description{
Gets the station data for the station closest to the given longitude and latitude
}
