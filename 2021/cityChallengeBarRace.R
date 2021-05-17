library(tidyverse)
library(glue)
library(jsonlite)
library(gganimate)
library(lubridate)
library(here)


# Load Data ---------------------------------------------------------------
# Graz Project
project_id <- 93064L

page <- 1L
obs_url <- glue("https://api.inaturalist.org/v1/observations?project_id={project_id}&locale=de&per_page=100&order=desc&order_by=created_at&page={page}")
obs_df <- fromJSON(obs_url)
obs_result <- list()
obs_result[[page]] <- obs_df$results # first result set
# calculate pages needed
pages <- ceiling((obs_df$total_results - 100) / 100)
page <- page + 1
for(i in page:pages){
  obs_url <- glue("https://api.inaturalist.org/v1/observations?project_id={project_id}&locale=de&per_page=100&order=desc&order_by=created_at&page={page}")
  obs_df <- fromJSON(obs_url)
  obs_result[[page]] <- obs_df$results # first result set
  print(glue("Page {page} from {pages} loaded"))
  page <- page + 1
}

rm(i, page, pages, obs_df)
obs_result <- bind_rows(obs_result)

# Data Wrangling ----------------------------------------------------------
obs_data <- obs_result %>% 
  select(time_observed_at, uuid) %>% 
  add_column(
    user_login = obs_result$user$login,
    taxon_id   = obs_result$taxon$id,
    taxon_name = obs_result$taxon$name
  ) %>%
  mutate(
    # time for sorting
    time = parse_date_time(
      time_observed_at, 
      "%Y-%m-%dT%H:%M:%S%z",
      exact = T
    ) %>% with_tz(tz = "CET"),
    time_shift = time %>%
      str_remove_all("[:-]") %>% 
      str_remove_all(" ") %>% 
      str_sub(6, 12) %>% as.integer(),
    time_date = time %>% round_date("1 minute") %>% format("%Y-%m-%d %H:%M"),
    count = 1 # helper count
  )  %>% 
  drop_na(time_date) %>% 
  arrange(time_date) %>% 
  group_by(time_date, user_login) %>% 
  summarise(
    # user count
    count      = sum(count)
    #user_count = cumsum(count)
  ) %>%
  ungroup() %>% 
  group_by(user_login) %>% 
  mutate(
    user_count = cumsum(count)
  )

# Create Dummy List of all Day/Times
dummy <- seq.POSIXt(as.POSIXct("2021-04-30 00:01"), as.POSIXct("2021-05-03 23:59"), by = "1 min") %>% 
  format("%Y-%m-%d %H:%M")

# %>% 
  # as.character %>% 
  #str_remove_all("[:-]") %>% 
  #str_remove_all(" ") %>% 
  #str_sub(6, 12) %>% 
  #as.integer()


full_hours <- cross2(
    c("0", dummy),
    unique(obs_data$user_login)
  ) %>% 
  map(
    setNames, c("time_date", "user_login")
  ) %>% 
  bind_rows

# https://community.rstudio.com/t/trying-to-create-animated-bar-plot-with-sliding-bars-that-overtake-each-other/46528/5

obs_plot <- full_hours %>%
  left_join(obs_data) %>% 
  mutate(
    user_count = if_else(time_date == "0", 0, user_count)
  ) %>% 
  arrange(user_login, time_date) %>% 
  fill(user_count) %>% 
  ungroup() %>% 
  group_by(time_date) %>%
  mutate(
    rank   = as.numeric(row_number(-user_count)),
    label  = if_else(rank > 15, "", paste(user_login, " ")),
    label2 = if_else(rank > 15, "", paste(" ", user_count)),
    user_count = if_else(rank > 15, 0, user_count),
    user_count2 = user_count / max(user_count) * 100, # percentage of max
    user_count2 = replace_na(user_count2, 0),
    rank = if_else(rank > 15, 15, rank)
  ) %>%
  ungroup() %>% 
  group_by(user_login) %>% 
  filter(str_count(str_c(label, collapse = "")) != 0) %>% 
  ungroup() %>% 
  arrange(time_date, rank)

obs_plot <- obs_plot %>% filter(time_date != "0")

p <- obs_plot %>% 
  ggplot(aes(group = as.factor(user_login))) +
  geom_tile(
    aes(
      x = rank,
      y = user_count2 / 2, 
      height = user_count2,
      fill = user_login
      ), 
    width = 0.9, # these aesthetics are constant
    alpha = 1 # hide bars for rank > 10    
    ) +
  geom_text(
    aes(
      x = rank,
      y = 0, 
      label = label
      ), 
    vjust = 0.2, 
    hjust = 1
    ) + 
  geom_text(
    aes(x = rank, y = user_count2, label = label2),
    vjust = 0.2,
    hjust = 0
    ) +
  coord_flip(
    clip = "off", 
    expand = FALSE,
    xlim = c(15.55, 0.45)
    ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE)  +
  theme_classic() +
  theme(
    #axis.line = element_blank(),
    axis.text =element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    #panel.border = element_blank(),
    panel.grid.major.x = element_line( size = 0.1, color = "grey" ),
    plot.margin = margin(2, 2, 2, 4, "cm")
  ) +
  labs(
    title = '{current_frame}',
    subtitle  =  "Top 15 Observer",
    caption  = "iNaturalist - City Challenge Graz 2021 (c) Hannes Oberreiter, Anna Dünser",
    x = "",
    y = ""
    )

anim <- p + transition_manual(frames = time_date)

anim_save(
  "bar_race_full.mp4", 
  anim, 
  renderer = ffmpeg_renderer(), 
  width = 1280, height = 720, 
  nframes = length(unique(obs_plot$time_date)), 
  fps = 20, 
  end_pause = 30
  )
