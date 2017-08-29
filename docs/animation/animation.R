library(tidyverse)
library(trafficaccidents)
library(viridis)

event_wd <-
  event %>%
  filter(!is.na(pos_x)) %>%
  mutate(alpha = 0.2) %>%
  left_join(calendar, by = "date") %>%
  filter(working_day == "Working day")

shift_hour <- function(x, n, a) {
  mutate(x, alpha = a, hour = (hour + n) %% 24)
}

dat_wd_shift <-
  bind_rows(
    event_wd, shift_hour(event_wd, 1, 0.1), shift_hour(event_wd, 2, 0.07)
  ) %>%
  arrange(hour, desc(road_type))

clock <- list(x = 378938 + 30000, y = 191590 - 10000, width = 8500)

p1 <-
  ggplot(data = dat_wd_shift, aes(frame = hour)) +
  geom_point(aes(pos_y, pos_x, alpha = alpha, color = road_type), size = 0.4) +
  scale_alpha_identity() +
  scale_color_viridis(discrete = TRUE, end = 0.7, option = "A", guide = "none",
                      limits = levels(dat_wd_shift$road_type)) +
  coord_fixed() +
  geom_text(aes(label = sprintf("%02d", hour)), color = "gray",
            x = clock$x, y = clock$y, size = 9) +
  geom_segment(size = 2, aes(
    x = clock$x + clock$width * sin(hour/6 * pi),
    y = clock$y + clock$width * cos(hour/6 * pi),
    xend = clock$x + clock$width*1.4 * sin(hour/6 * pi),
    yend = clock$y + clock$width*1.4 * cos(hour/6 * pi),
    color = levels(dat_wd_shift$road_type)[as.integer(hour >= 12)*8+1]
  )) +
  theme_void()


library(gganimate)
library(animation)

magickPath <- shortPathName("magick.exe")
ani.options(convert=magickPath)
#ani.options(ani.width = 1024, ani.height = 640+55)
ani.options(ani.width = 1024*1.3, ani.height = (640+55)*1.3)
ani.options(interval = 0.1)
gganimate(p1, "docs/animation/accidents_animation.gif", title_frame = FALSE )



