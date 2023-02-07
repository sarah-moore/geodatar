################################
## GEOSPATIAL DATA + VIZ IN R ##
## Workshop by: Sarah Moore   ##
################################

# Task One: Install + Load Relevant Packages

# Important packages for today: 

packages <- c("tidyverse", "ggmap", "sf", "rnaturalearth",
              "rnaturalearthdata", "leaflet", "tidycensus",
              "rgeos")

# Uncomment this to install:
#install.packages(packages, dep = T)

# Load all the packages in one command: 
invisible(lapply(packages, library, character.only = TRUE))

# Sometimes, the spatial packages can be difficult, if you have a problem let me know.
# This may also work to download the sf project if the first option does not work: 

library(remotes)
#install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")

######################
# Task Two: Peruse the Built-In Options 

# From the rnaturalearth package:

# 1) Map of the world 

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf()-> world_map

world_map

###
# 2) Map of the world with a mapped variable (already in the rnaturalearth dataset)
world$economy_c <- as.numeric(str_sub(world$economy, end= 2))

world$economy_c <- abs(world$economy_c-8)

ggplot(data = world, aes(fill = economy_c))+
  geom_sf(color = "black", lwd= 0.2) +
  scale_fill_gradient(low = "orange", high = "darkgreen",
                      name ="Economic Development",
                      breaks = c(1, 4, 7),
                      labels = c("Low", "Moderate","High")) +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.title = element_text(size = 8)) +
  labs(title = "National Level Economic Development")-> econ_world

econ_world

###
# 3) Isolate geounits of interest 

brazil <- ne_countries(geounit = "brazil", 
                       returnclass = "sf")

ggplot(data = brazil)+
  geom_sf(fill = "#19AE47", 
          alpha = 0.5) +
  theme(
    axis.text = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(x = "", y = "", title = "Brazil")->brazil_solo

brazil_solo

### 
# 4) Or, highlight selected countries among its neighbors

s_america <- ne_countries(continent = "south america", 
                          returnclass = "sf")

s_america$brazil <- if_else(s_america$geounit == "Brazil", 
                            1, 0)

ggplot(data = s_america, aes(fill = 
                               as_factor(brazil)))+
  geom_sf(alpha = 0.5) +
  scale_fill_manual(values = c("lightgrey", 
                                          "#19AE47")) +
  annotate('text', x = -50, y = -12, 
           label = "Brazil", size = 4, 
           family = "serif") +
  theme(
    axis.text = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "") -> s_america_gg

s_america_gg

###
# 5) Can also map the subnational administrative units of a country

brazil_states <- ne_states(country = "brazil", returnclass = "sf")

ggplot(data = brazil_states)+
  geom_sf() + 
  theme(
    axis.text = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "", 
       title = "States of Brazil")-> brazil_subnat

brazil_subnat

######################
# Task 3: Import and Use Shapefiles from Other Sources

chicago_schools <- st_read("data/school/chicago-schools.shp", 
                           quiet = T)

# Let's map that shapefile
ggplot(data = chicago_schools)+ 
  geom_sf() + 
  theme(
    axis.text = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )-> chicago_schools_map

chicago_schools_map # What's the issue? 

# Add in the wards: 

chicago_wards <- st_read("data/chicago/chicago.shp", 
                         quiet = T)

# Now map those

ggplot(data = chicago_wards)+ 
  geom_sf() + 
  theme(
    axis.text = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )-> chicago_wards_map

chicago_wards_map # Looking better! Now layer the schools.

ggplot() +
  # Add base layer 
  geom_sf(data = chicago_wards, 
          fill = "white", color = "black") +
  # And then the other 
  geom_sf(data = chicago_schools, 
          aes(color = governance),
          alpha = 0.5) + 
  scale_color_discrete(name = "Governance Type") + 
  theme(
    axis.text = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
  ) + 
  labs(title = "Chicago School Locations and Governance Types"
       )-> chicago_gov_schools

chicago_gov_schools


######################
# Exercise 1 

# 1) Install and load all of the packages on to your local machine. 
# (Hint: Start out with the R script title "geodatar_exercises.R")

# 2) Import the Chicago Park District shapefile from the data subfolder "parks." 

# 3) Create a layered map: 
#   (a) Chicago city wards in white 
#   (b) Chicago parks in dark green. 

# 4) Using some dplyr skills, as well as `st_join()` from the `sf` package
#   (a) count the number of parks per ward OR sum the total acres of park space per ward
#   (b) fill in the ward with a gradient green based on the mutated variable
# HINT: Prior to using the `st_join()` function, you will probably have to use this code to override some settings: `sf_use_s2(FALSE)`
######################
# Task 4: Orient yourself with ways to ensure map projections are equivalent 

options(tigris_use_cache = TRUE)
nm_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "NM", 
  year = 2020, 
  geometry = T
)

nm_income
st_crs(nm_income)

ggplot() +
  geom_sf(data = nm_income)

land_grants <- st_read("data/NM_grant_lands/nm_grant_lands.shp", 
                       quiet = T)

ggplot() +
  geom_sf(data = land_grants)

# check the line "geodetic CRS" 
land_grants
st_crs(land_grants)

# layer it together: 

ggplot() +
  geom_sf(data = nm_income) + 
  geom_sf(data = land_grants)


# all checks out :) but in case it doesn't, can use st_transform()  
# ggplot, fortunately, will always reproject for you! 

######################
# Task 5: Conducting some real spatial analysis (?)

ggplot() +
  geom_sf(data = nm_income, 
          aes(fill = estimate)) + 
  scale_fill_gradient2(
    low = "white", 
    high = "darkgreen"
  )-> nm_wealth

nm_wealth

# create a distance matrix between centroids and the land grant features 
sf_use_s2(FALSE)
nm_dist_mat <- nm_income %>%
  st_centroid() %>%
  st_distance(land_grants) 

# glimpse at the matrix 
nm_dist_mat[1:10, 1:10]

# compute mean distance of each census tract and create a vector of the kilometer 
mean_dist <- nm_dist_mat %>%
  apply(1, mean) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

# make sure dimensions are equal on length 
dim(nm_income)
dim(mean_dist)

# bind the distance vector to the spatial data
nm_income <- cbind(nm_income, mean_dist)

# can visualize as a histogram 
ggplot(nm_income, aes(x= mean_dist)) + 
  geom_histogram()

# and also as a map itself 
ggplot() + 
  geom_sf(data = nm_income, aes(fill = mean_dist)) + 
  geom_sf(data = land_grants)


######################
# Exercise 2 

# 1) Make sure you have both the NM census data and the land grant data. 

# 2) Compute and filter the census data to only include tracts 
# in the top 25 percentile of income (the variable name is `estimate`). 

# 3) Compute a distance matrix and then create a vector of *minimum* distances. 
# (HINT: change the function in your `apply()` command.)

# 4) View a histogram of these minimum distances. 

# 5) View a mapped plot where the fill is the minimum distance of a census tract to a land grant. 
######################

# Bonus option 

chicago_ward_parks <- cbind(chicago_ward_parks, 
                            st_coordinates(st_centroid(chicago_ward_parks$geometry)))

chicago_ward_parks %>%
  leaflet() %>%
  setView(-87.6298, 41.8781, zoom = 11) %>%
  addTiles() %>%
  addMarkers(~X, ~Y, 
             clusterOptions = markerClusterOptions()) 


chicago_ward_parks %>%
  leaflet() %>%
  setView(-87.6298, 41.8781, zoom = 11) %>%
  addTiles() %>%
  addMarkers(~X, ~Y)
