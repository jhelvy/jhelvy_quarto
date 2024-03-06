library(ggplot2)
library(magick)

# Function to calculate hexagon vertices

deg2rad <- function(deg) { return(deg * (pi / 180)) }

hexagon_vertices <- function(center_x, center_y, radius) {
  angles_deg <- seq(90, -270, by = -60) # Starting from the top vertex and moving clockwise
  angles_rad <- deg2rad(angles_deg)
  x <- center_x + radius * cos(angles_rad)
  y <- center_y + radius * sin(angles_rad)
  return(data.frame(x = x, y = y))
}

# Make plot 

favicon <- ggplot() + 
    # Make outer hexagon
    geom_polygon(
        data = round(hexagon_vertices(0, 0, 3), 6), 
        aes(x = x, y = y), 
        fill = '#2c3f51'
    ) +
    # Make inner hexagon
    geom_polygon(
      data = round(hexagon_vertices(0, 0, 2.7), 6), 
      aes(x = x, y = y), 
      fill = '#5480a7'
    ) +
    # Add J
    geom_text(
      aes(x = 0, y = 0, label = 'j'), 
      hjust = 0.5, 
      nudge_y = 0.4,
      size = 90,
      family = 'Raleway', 
      fontface = 'bold', 
      color = 'white'
    ) +
    coord_fixed() +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
    )

ggsave(
  'favicon.pdf', favicon, height = 6, width = 6, 
  bg = "transparent", device = cairo_pdf
)
renderthis::to_png('favicon.pdf', 'favicon.png', density = 300)
favicon <- image_scale(image_read('favicon.png'), '32')
image_write(favicon, path = "favicon.ico", format = "png")
