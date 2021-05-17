library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(sf)
library(gganimate)
library(lubridate)
library(here)
library(av)

# Map ---------------------------------------------------------------------
mapDataPath <- glue("{here()}/data/maps.RData")
# Read MAPS
if (!exists("mfStates")) {
  if (file.exists(mapDataPath)) {
    load(mapDataPath)
  } else {
    mapAustria <- read_sf(glue("{here()}/map"))
    mfStates <- mapAustria %>%
      group_by(BL) %>%
      summarize(
        geometry = st_union(geometry)
      )
    mfStatesSimplify <- mfStates %>% st_simplify(dTolerance = 0.002)
    mfDistricts <- mapAustria %>%
      group_by(PB) %>%
      summarize(
        geometry = st_union(geometry)
      )
    mfDistrictsSimplify <- mfDistricts %>% st_simplify(dTolerance = 0.002)
    # save R object to prevent loading each time
    save(mapAustria, mfDistricts, mfStates, mfStatesSimplify, mfDistrictsSimplify, file = mapDataPath)
  }
}
rm(mapDataPath)



# Load Data ---------------------------------------------------------------
# Graz Project
project_id <- 93064L

project_url <- glue("https://api.inaturalist.org/v1/projects/{project_id}")
project_df <- fromJSON(project_url)

page <- 1L
obs_url <- glue("https://api.inaturalist.org/v1/observations?project_id={project_id}&locale=de&per_page=200&order=desc&order_by=created_at&page={page}")
obs_df <- fromJSON(obs_url)
obs_result <- list()
obs_result[[page]] <- obs_df$results # first result set
# calculate pages needed
pages <- ceiling((obs_df$total_results - 200) / 200)
page <- page + 1

for(i in page:pages){
  obs_url <- glue("https://api.inaturalist.org/v1/observations?project_id={project_id}&locale=de&per_page=200&order=desc&order_by=created_at&page={page}")
  obs_df <- fromJSON(obs_url)
  obs_result[[page]] <- obs_df$results # first result set
  page <- page + 1
}
rm(i, page, pages)
obs_result <- bind_rows(obs_result)
obs_raw <- obs_result
obs_taxon <- obs_result$taxon


# Map Coords --------------------------------------------------------------
map_point <- map(obs_result$geojson$coordinates, st_point)
map_point <- st_sfc(map_point)
st_geometry(obs_result) <- map_point
obs_result <- obs_result %>% 
  separate(location, c("lat", "long"), ",", convert = T)

# set crs from map
st_crs(obs_result) <- st_crs(mfStatesSimplify)

# Time  -------------------------------------------------------------------

obs_result <- obs_result %>% 
  mutate(
    time = parse_date_time(
      time_observed_at, 
      "%Y-%m-%dT%H:%M:%S%z",
      exact = T
    ) %>% with_tz(),
    month = lubridate::month(time) %>% as.numeric(),
    day = lubridate::day(time) %>% as.numeric(),
    hour = lubridate::hour(time) %>% as.integer()
  )

obs_plot <- bind_cols(obs_result, obs_taxon, obs_result$user)
obs_plot <- obs_plot %>% drop_na(hour, iconic_taxon_name)

p <- obs_plot %>% 
  ggplot(aes(x = long, y = lat, group = uuid, color = login)) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_sf(
    data = mfDistrictsSimplify, 
    aes(geometry = geometry), inherit.aes = F, alpha = 0) +
  coord_sf(
    xlim = c(15, 15.8), ylim = c(46.8, 47.3), expand = FALSE
    ) +
  facet_wrap(~ iconic_taxon_name) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  xlab("") + ylab("") + 
  transition_reveal(hour) + 
  labs(
    title = "City Challenge Graz, 30. April - Stunde: {frame_along}",
    caption = "Distinkte Farben pro BeobachterIn"
    )
  
anim_save(
  "first_saved_animation.gif", 
  animation = p,
  end_pause = 10, 
  rewind = FALSE,
  height = 7,
  width = 10,
  units = "in",
  res = 100
  )

anim_save(
  "first_saved_animation.mp4", 
  animation = p,
  renderer = av_renderer()
)


