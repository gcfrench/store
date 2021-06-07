# example taken from palmerpenguins example analysis of mass vs. flipper length
# https://allisonhorst.github.io/palmerpenguins/articles/examples.html

suppressPackageStartupMessages({
  library(store)
  suppressWarnings({
    library(palmerpenguins)
    library(fs)
    library(dplyr)
    library(ggplot2)
  })
})

# create temp directory
dir_create(path(tempdir(), "figures"))

# data
penguins_mass_flipper <- penguins %>%
  select(species, flipper_length_mm, body_mass_g)

# graph
penguins_mass_flipper_plot <- ggplot(data = penguins_mass_flipper,
                                     aes(x = flipper_length_mm,
                                         y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

# save graph
suppressWarnings({
  ggsave(filename = path(tempdir(), "figures", "penguins_mass_flipper_plot.png"),
       plot = penguins_mass_flipper_plot,
       type = "cairo-png",
       width = 6,
       height = 6,
       units = "in",
       dpi = 72)
})

