
library(ggplot2)
library(hexSticker)
library(cropcircles)

navy      <- "#082B55"
navy_dark <- "#031D3D"
teal      <- "#0D8995"
seafoam   <- "#9ED9CF"
white     <- "#FFFFFF"

# Population trajectory and estimated reference points.
curve <- data.frame(
  x = seq(0.10, 0.88, length.out = 120)
)
curve$y <- 0.18 + 0.50 * curve$x^1.65

# Layered wave crests along the lower edge, matching the original sticker.
waves_back <- data.frame(x = seq(0, 1, length.out = 500))
waves_back$crest <- 0.235 +
  0.012 * sin(10 * pi * waves_back$x) +
  0.005 * sin(20 * pi * waves_back$x)

waves_front <- data.frame(x = seq(0, 1, length.out = 500))
waves_front$crest <- 0.205 +
  0.010 * sin(10 * pi * waves_front$x + pi / 2)

ref_x <- c(0.18, 0.50, 0.82)
refs <- data.frame(
  x = ref_x,
  y = approx(curve$x, curve$y, xout = ref_x)$y
)

artwork <- ggplot() +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
    fill = navy
  ) +
  geom_ribbon(
    data = waves_back,
    aes(x = x, ymin = -Inf, ymax = crest),
    inherit.aes = FALSE,
    fill = "#167A88",
    colour = NA
  ) +
  geom_ribbon(
    data = waves_front,
    aes(x = x, ymin = -Inf, ymax = crest),
    inherit.aes = FALSE,
    fill = teal,
    colour = NA
  ) +
  geom_hline(
    yintercept = 0.22,
    colour = white,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_line(
    data = curve,
    aes(x, y),
    colour = white,
    linewidth = 1.8,
    lineend = "round"
  ) +
  geom_segment(
    data = refs,
    aes(x = x, xend = x, y = 0.22, yend = y),
    colour = white,
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  geom_point(
    data = refs,
    aes(x, y),
    shape = 21,
    size = 3,
    stroke = 1.8,
    colour = white,
    fill = seafoam
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.12, 1)) +
  theme_void()

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

hexSticker::sticker(
  subplot = artwork,
  package = "om",
  p_size = 100,
  p_family = "sans",
  p_fontface = "bold",
  p_color = white,
  p_x = 1,
  p_y = 0.6,
  s_x = 1,
  s_y = 1.3,
  s_width = 1.8,
  s_height = 1.5,
  h_fill = teal,
  h_color = white,
  h_size = 0,
  spotlight = FALSE,
  url = "",
  filename = "man/figures/logo.png",
  dpi = 1200,
  white_around_sticker = FALSE
); dev.off()

crop_hex(
  images = "man/figures/logo.png",
  to = "man/figures/logo.png",
  border_size = 0,
  border_colour = "white",
  just = "center"
)

message("Created 'man/figures/logo.png'")

