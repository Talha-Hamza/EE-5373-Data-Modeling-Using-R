library(ggplot2)
library(gganimate)
library(gifski)


which_state <- "california"
county_info <- map_data("county", region=which_state)

base_map <- ggplot(
  data = county_info,
  mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "white") +
  coord_quickmap() +
  theme_void()


min_long <- min(county_info$order)
max_long <- max(county_info$order)

map_with_data <- base_map + geom_point(data = county_info,
                aes(x = long, y = lat, color=temp, size=temp, group=region)) +
                coord_quickmap(xlim = c(min_long, max_long))

map_with_data
