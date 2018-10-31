# Make sure you install packages!
# I recommend dev version
#
# install.packages('devtools')
# devtools::install_github("dkahle/ggmap")

library(ggmap)
register_google("API KEY HERE")

library(tidyverse)

example_df <- tibble(
  ae               = c("John Smith","Barack Obama", "Jane Doe")
  ,home_city       = c("Redding","Chicago","Bathesda")
  ,home_state      = c("CA","IL","MD") # should also work with full state name
  ,territory_state = c("NV","TX","NY") # geocode will grab center of state
  ,avg_monthly_ot  = c(100, 500,  300)
  ,avg_monthly_te  = c(1000, 5000, 3000)
)


# This will give you a lot of info, including lat/lon of the center of the state
# good idea to take a look to see if it geocoded correctly
territories_geocoded <- example_df %>%
  mutate(search_key = territory_state) %>%
  select(ae, territory_state, search_key) %>%
  mutate_geocode(search_key, output = 'more')


# this will now give you lat lon of the center of the city
# normally take a look to see if it geocoded correctly
homes_geocoded <- example_df %>%
  mutate(search_key = paste(home_city, home_state, sep = ", ")) %>%  # search will be "Redding, CA"
  select(ae, home_state, home_city, search_key) %>%
  mutate_geocode(search_key, output = 'more')

# Now that we have both homes an territories geocoded we can combine the lat/lons
homes_lat_lon <- homes_geocoded %>%
  select(
     ae # technically only need this for the join
    ,home_state
    ,home_city
    ,home_lat = lat
    ,home_lon = lon
  )

territories_lat_lon <- territories_geocoded %>%
  select(
     ae
    ,territory_state
    ,territory_lat = lat
    ,territory_lon = lon
  )


# Join them to original
geocoded_df <- example_df %>%
  left_join(homes_lat_lon) %>%
  left_join(territories_lat_lon)


# Now we need to calculate distance
#
# Halversine Distance
#
# So the Earth is round, therefore you can't just take the distance between two points
# Instead you need to calculate the distance along the arc between two points
# There are a few ways to do this - the most common way is the Halversine Distance
#
# You can do that with the geosphere package

# install.pacakges('geosphere')
library(geosphere)

# it returns meters so we'll need a conversion factor
meters_to_miles <- 0.000621371 # lol Google'd it

distance_df <- geocoded_df %>%
  mutate(
    distance = distHaversine(
        cbind(home_lon, home_lat)
      , cbind(territory_lon, territory_lat)
    ) # calculate distance between 2 points on spherical Earth
    , distance_in_miles = distance * meters_to_miles
  )


