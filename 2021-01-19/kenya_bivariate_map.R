
library(tidyverse)
library(sf)
library(ggrepel)
library(glue)
library(cowplot)
library(rKenyaCensus)
library(extrafont)

# --- load the shapefile data

shp_kenya <- st_as_sf(rKenyaCensus::KenyaCounties_SHP) %>%
  mutate( # centroids to label counties
    CENTROID = map(geometry, st_centroid), # apply s_centroid() function to each element in geometry list
    COORDS = map(CENTROID, st_coordinates), # apply st_coordinates() function to each element in geometry list
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2),
    nudge_x = 0,
    nudge_y = 0
  ) %>%# cleaning 
  mutate( 
    Population = as.numeric(as.character(Population)), # convert Population from factor to double
    Population = ifelse(Population != 0, Population, NA), # convert 0 pop to NA
    County_lower = str_to_lower(County),
    County_title = str_to_title(County),
  ) %>% 
  as_tibble() %>%
  st_as_sf()

### --- ADD COUNTY NAMES AS LABELS
# need the range of the coordinates, we want to scale the nudge by the range.
x_range <- abs(range(shp_kenya$COORDS_X)[2] - range(shp_kenya$COORDS_X)[1]) # alternative way: abs(Reduce("-", range(shp_kenya$COORDS_X)))
y_range <- abs(range(shp_kenya$COORDS_Y)[2] - range(shp_kenya$COORDS_Y)[1]) # alternative way: abs(Reduce("-", range(shp_kenya$COORDS_X)))

# we add a custom nudge for some of the labels:
#   inspiration: https://github.com/slowkow/ggrepel/issues/89
shp_kenya <- shp_kenya %>%
  mutate(
    nudge_x = case_when(
      County == "KISII" ~ -0.4 * x_range, 
      County == "BUSIA" ~ -0.1 * x_range,
      County == "BUNGOMA" ~ -0.05 * x_range,
      # County == "KERICHO" ~ -0.4 * x_range,
      County == "SAYA" ~ -0.1 * x_range,
      County == "ELGEYO/MARAKWET" ~ -0.4 * x_range,
      County == "NAIROBI CITY" ~ -0.05 * x_range,
      County == "THARAKA-NITHI" ~ 0.57 * x_range,
      County == "MOMBASA" ~ 0.08 * x_range,
      # County == "MURANG'A" ~ -0.3 * x_range,
      County == "KIAMBU" ~ -0.16 * x_range,
      County == "TTRANS NZOIA" ~ -0.05 * x_range,
      County == "VIHIGA" ~ -0.35 * x_range,
      County == "EMBU" ~ 0.01 * x_range,
      TRUE ~ nudge_x
    ),
    nudge_y = case_when(
      County == "NAIROBI CITY" ~ -0.2 * y_range,
      County == "KIRINYAGA" ~ -0.4 * y_range,
      # County == "MURANG'A" ~ -0.2 * y_range,
      County == "NYAMIRA" ~ -0.15 * y_range,
      County == "KIAMBU" ~ -0.19 * y_range,
      County == "ELGEYO/MARAKWET" ~ 0.15 * y_range,
      # County == "TRANS NZOIA" ~ -0.1 * y_range,
      County == "NANDI" ~ -0.005 * y_range,
      County == "KAKAMEGA" ~ -0.008 * y_range,
      County == "NYERI" ~ -0.008 * y_range,
      # County == "KERICHO" ~ -0.3 * y_range,
      County == "NYANDARUA" ~ 0.02 * y_range,
      TRUE ~ nudge_y
    )
  )

glimpse(shp_kenya)

# --- load the livestock data

livestock <- rio::import("V4_T2.24.csv") %>%
  filter(AdminArea == "County") # map at the county level, no SubCounty
glimpse(livestock)

# --- Join shapefile and livestock data

# join the HCI data to the sf data:
livestock_kenya <- left_join(shp_kenya, livestock, by = "County")
glimpse(livestock_kenya)


# --- Build quantiles

# function to transform the variables of interest to quantiles:
# (inspired by https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/)
quantilize <- function(data, my_var, no_classes = 3){
  # see this: https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/
  quantiles <- data %>%
    pull({{my_var}}) %>%
    quantile(probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE) %>%
    as.vector() # to remove names of quantiles, so idx below is numeric
  # we create custom labels for the legend:
  labels <- imap_chr(quantiles, 
                     function(., idx){
                       return(paste0(round(quantiles[idx], 2),
                                     " – ",
                                     round(quantiles[idx + 1], 2)
                       ))
                     }
  )
  # we need to remove the last label (we have 6 breaks but 5 intervals):
  labels <- labels[1:length(labels) - 1]
  # And we create a new variable in the dataset with the quantiles
  data %>%
    mutate(
      "{{my_var}}_{{no_classes}}" := cut({{my_var}},
                                         breaks = quantiles,
                                         labels = labels,
                                         include.lowest = T)
    )
}



# divide vars in 3 quantiles == add as a new column with suffix '_3':
livestock_kenya <- quantilize(livestock_kenya, 
                              my_var = IndigenousCattle, 
                              no_classes = 3) # note: adds a new column, so can re-use hci_kenya
livestock_kenya <- quantilize(livestock_kenya, 
                              my_var = Goats, 
                              no_classes = 3) # note: adds a new column, so can re-use hci_kenya

# we create a grouped variable, which name matches the bivariate color scale below
livestock_kenya <- livestock_kenya %>% mutate(
  group_cattle_goat = paste(
    as.numeric(IndigenousCattle_3), "-", # as numeric, so 'factor' from the quantile (1,2 or 3)
    as.numeric(Goats_3)
  )
)

# create color scale that encodes two variables
# red for cattle and blue for goats
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#804D36", # "#3F2949", # high cattle, high goat
  "2 - 3" = "#966B82",# "#435786",
  "1 - 3" = "#9972AF",# "#4885C1", # low cattle, high goat
  "3 - 2" = "#AF8D53",# "#77324C",
  "2 - 2" = "#C8ADA0",# "#806A8A", # medium cattle, medium goat
  "1 - 2" = "#CBB9D7",# "#89A1C8",
  "3 - 1" = "#C8B35A",# "#AE3A4E", # high cattle, low goat
  "2 - 1" = "#E4D8AC",# "#BC7C8F",
  "1 - 1" = "#E8E8E8",# "#CABED0" # low cattle, low goat
) %>%
  gather("group", "bivariate_fill")

# and then we join the bivariate color scale:
livestock_kenya <- livestock_kenya %>% left_join(bivariate_color_scale, by = c("group_cattle_goat" = "group")) %>%
  rename(bivariate_cattle_goat = bivariate_fill)

# --> we have all the data needed for plotting the map!


# --- Styling

map_boundaries <- 'black' # "darkgrey" # color of county borders
my_font <- "Fira Sans"

my_map_theme <- function() {
  theme_void() +
    theme(plot.background = element_rect(color = "white"),
          legend.position = "right",
          legend.text = element_text(family = my_font, size = 15),
          legend.title = element_text(family = my_font, size = 15, face = "bold"),
          plot.caption = element_text(family = my_font, size = 7),
          plot.title = element_blank(), # element_text(family = my_font, size = 13, hjust = 0),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm")
    )
}


# --- Plot the map

the_map <- ggplot(livestock_kenya, aes(fill = bivariate_cattle_goat)) +
  geom_sf(color = map_boundaries, size = .1) +
  # as the sf object bivariate_fill has a column that contains the literal color 
  # as hex code for each county, we can use scale_fill_identity here
  scale_fill_identity() +
  geom_text_repel(
    mapping = aes(x = COORDS_X, y = COORDS_Y, label = County_title),
    nudge_x = livestock_kenya$nudge_x,
    nudge_y = livestock_kenya$nudge_y,
    size = 3.2,
    min.segment.length = 1,
    point.padding = 0, # Amount of padding around labeled point, as unit or number. Defaults to 0
    force = 0, # Force of repulsion between overlapping text labels. Defaults to 1.
    force_pull = 100, # Force of attraction between a text label and its corresponding data point. Defaults to 1.
    segment.color = "black", #"grey50"
    segment.size = 0.2
  ) +
  labs(
    caption = "Map by François Delavy - @f_delavy | Data by rKenyaCensus - github.com/Shelmith-Kariuki/rKenyaCensus"
  ) +
  my_map_theme()


# and to  --- plot the legend:
# separate the groups
bivariate_color_scale_legend <- bivariate_color_scale %>% # be really careful with the order of the variables:
  separate(group, into = c("Cows", "Goats"), sep = " - ") %>%
  mutate(Cows = as.integer(Cows),
         Goats = as.integer(Goats))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale_legend,
    mapping = aes(
      x = Cows,
      y = Goats,
      fill = bivariate_fill)
  ) +
  scale_fill_identity() +
  labs(x = paste("More Cows", sprintf('\u2192')),
       y = paste("More Goats", sprintf('\u2192'))) +
  my_map_theme() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()



# Put everything together:

bivariate_map <-ggdraw() +
  draw_plot(the_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)

ggsave(filename = "bivariate_map_livestock.png",
       plot = bivariate_map,
       width = 200,
       height = 200,
       units = "mm",
       dpi = 600)





