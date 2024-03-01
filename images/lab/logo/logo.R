library(ggplot2)
library(Cairo)

# Function to calculate hexagon vertices

deg2rad <- function(deg) { return(deg * (pi / 180)) }

hexagon_vertices <- function(center_x, center_y, radius) {
  angles_deg <- seq(90, -270, by = -60) # Starting from the top vertex and moving clockwise
  angles_rad <- deg2rad(angles_deg)
  x <- center_x + radius * cos(angles_rad)
  y <- center_y + radius * sin(angles_rad)
  return(data.frame(x = x, y = y))
}

# Calculate vertices
radius <- 3
hexagon_points <- round(hexagon_vertices(0, 0, radius), 6)

# Calculate lines from center
segments_data <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  xend = hexagon_points$x[c(2, 4, 6)],
  yend = hexagon_points$y[c(2, 4, 6)]
)

# Add 3D letters ----

offset <- 0.6

# Find points to make "T"

mid_x <- hexagon_points$x[6] / 2
mid_y <- hexagon_points$y[6] / 2
shift_x <- offset/2*cos(deg2rad(30))
shift_y <- offset/2*sin(deg2rad(30))

t_points <- data.frame(
  x = c(
    0, 
    0, 
    mid_x + shift_x, 
    mid_x + shift_x, 
    mid_x - shift_x, 
    mid_x - shift_x, 
    hexagon_points$x[6], 
    hexagon_points$x[6]
  ),
  y = c(
    0,
    -offset, 
    mid_y - offset - shift_y, 
    mid_y - radius - shift_y, 
    mid_y - radius + shift_y, 
    mid_y - offset + shift_y, 
    hexagon_points$y[6] - offset, 
    hexagon_points$y[6]
  )
)

# Find points to make "C"

shift_x <- offset*cos(deg2rad(30))
shift_y <- offset*sin(deg2rad(30))

c_points <- data.frame(
  x = c(
    0, 
    hexagon_points$x[6],
    hexagon_points$x[1], 
    hexagon_points$x[1] + shift_x,
    hexagon_points$x[6] + 2*shift_x, 
    0, 
    hexagon_points$x[2] - shift_x, 
    hexagon_points$x[2]
  ),
  y = c(
    0,
    hexagon_points$y[6],
    hexagon_points$y[1], 
    hexagon_points$y[1] - shift_y,
    hexagon_points$y[6], 
    offset, 
    hexagon_points$y[2] + shift_y, 
    hexagon_points$y[2]
  )
)

# Find points to make "L"

l_points <- data.frame(
  x = c(
    0, 
    hexagon_points$x[4], 
    hexagon_points$x[3], 
    hexagon_points$x[3],
    hexagon_points$x[4] + shift_x, 
    shift_x
  ),
  y = c(
    0, 
    hexagon_points$y[4], 
    hexagon_points$y[3], 
    hexagon_points$y[3] + offset,
    hexagon_points$y[4] + offset + shift_y, 
    shift_y
  )
)
 
# Logo (with no labels)

logo <- ggplot() + 
    # Make outer hexagon
    geom_polygon(
        data = hexagon_points, 
        aes(x = x, y = y), 
        fill = 'white', color = "black", 
        size = 2
    ) +
    # Make inner lines
    geom_segment(
        data = segments_data, 
        aes(x = x, y = y, xend = xend, yend = yend), 
        color = "black", 
        size = 2
    ) +
    # Make parallelograms
    geom_polygon(
        data = rbind(hexagon_points[c(4, 5, 6),], data.frame(x = 0, y = 0)),
        aes(x = x, y = y),
        color = "black", fill = "#103b5b",
        size = 2
    ) +
    geom_polygon(
        data = rbind(hexagon_points[c(6, 1, 2),], data.frame(x = 0, y = 0)),
        aes(x = x, y = y),
        color = "black", fill = "#b5d562",
        size = 2
    ) +
    geom_polygon(
        data = rbind(hexagon_points[c(2, 3, 4),], data.frame(x = 0, y = 0)),
        aes(x = x, y = y),
        color = "black", fill = "#c6d6e0",
        size = 2
    ) +
    # Make T
    geom_polygon(
        data = t_points,
        aes(x = x, y = y),
        color = "black", fill = "white",
        size = 2
    ) +
    # geom_point(data = t_points, aes(x = x, y = y), size = 5, color = 'white') +
    # Make C
    geom_polygon(
        data = c_points,
        aes(x = x, y = y),
        color = "black", fill = "white",
        size = 2
    ) +
    # geom_point(data = c_points, aes(x = x, y = y), size = 5, color = 'white') +
    # Make L
    geom_polygon(
        data = l_points,
        aes(x = x, y = y),
        color = "black", fill = "white",
        size = 2
    ) +
    # geom_point(data = l_points, aes(x = x, y = y), size = 5, color = 'white') +
    coord_fixed() +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
    )

ggsave('logo.pdf', logo, height = 6, width = 6, bg = "transparent", device = cairo_pdf)
renderthis::to_png('logo.pdf', 'logo.png', density = 300)

# Add labels 

logo_labeled <- logo +
  # Add Labels
  geom_text(
    aes(x = hexagon_points$x[6], y = hexagon_points$y[6], label = 'TECHNOLOGY'), 
    hjust = 0, 
    angle = -30, 
    size = 9, 
    nudge_x = 0.07, 
    nudge_y = -0.32,
    family = 'Raleway', 
    fontface = 'bold'
  ) +
  geom_text(
    aes(x = hexagon_points$x[1], y = hexagon_points$y[1], label = 'CHANGE'), 
    hjust = 1, 
    angle = 30, 
    size = 9, 
    nudge_x = 0.08, 
    nudge_y = -0.25,
    family = 'Raleway', 
    fontface = 'bold'
  ) +
  geom_text(
    aes(x = hexagon_points$x[3], y = hexagon_points$y[3], label = 'LAB'), 
    hjust = 1, 
    angle = 30, 
    size = 9, 
    nudge_x = -0.1, 
    nudge_y = 0.25,
    family = 'Raleway', 
    fontface = 'bold'
  ) +
  geom_text(
    aes(x = hexagon_points$x[5], y = hexagon_points$y[5], label = 'GWU'), 
    hjust = 0, 
    angle = -30, 
    size = 9, 
    nudge_x = 0.07, 
    nudge_y = 0.25,
    family = 'Raleway', 
    fontface = 'bold', 
    color = 'white'
  ) +
  geom_text(
    aes(x = hexagon_points$x[2], y = hexagon_points$y[2], label = 'EMSE'), 
    hjust = 1, 
    angle = 30, 
    size = 9, 
    nudge_x = -0.15, 
    nudge_y = -0.38,
    family = 'Raleway', 
    fontface = 'bold'
  ) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave('logo-labeled.pdf', logo_labeled, height = 6, width = 6, bg = "transparent", device = cairo_pdf)
renderthis::to_png('logo-labeled.pdf', 'logo-labeled.png', density = 300)
