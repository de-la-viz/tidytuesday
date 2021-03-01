
library(sf)
library(tidyverse)
library(rKenyaCensus)
library(extrafont)
library(scales)
library(ggrepel)

# to do:
# - NOT A PRIO: handle 	MURANG'A and TRANS NZOIA because values == 0 (for all factors, not only  population)
# - have the border with Sudan as ----

### --- LOAD SHAPEFILE

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
glimpse(shp_kenya)

# st_write(shp_kenya, "shapefiles/shp_kenya.shp")

# mask_boundary <- st_read("shapefiles/maskline_detailed_2013.shp")
# glimpse(mask_boundary)

### --- JOIN DATA TO BE PLOTTED

# here, no new data, but need to binarize the population

# we binarize the population by quantile:
# inspired by https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

# first define number of classes
no_classes <- 5

# extract quantiles  quantile(shp_kenya$Population, c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE)
quantiles <- shp_kenya %>%
  pull(Population) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric

# here we create custom labels for the legend:
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx] / 1000000, 1),
                "m",
                " – ",
                round(quantiles[idx + 1] / 1000000, 1),
                "m"))
})

# we need to remove the last label 
# because that would be something like "4397k - NA"
labels <- labels[1:length(labels) - 1]

# here we actually create a new 
# variable on the dataset with the quantiles
shp_kenya <- shp_kenya %>%
  mutate(
    Quant_Pop = cut(Population,
                    breaks = quantiles,
                    labels = labels,
                    include.lowest = T))


### --- ADD COUNTY NAMES AS LABELS

# need the range of the coordinates, we want to scale the nudge by the range.
x_range <- abs(range(shp_kenya$COORDS_X)[2] - range(shp_kenya$COORDS_X)[1]) # alternative way: abs(Reduce("-", range(shp_kenya$COORDS_X)))
y_range <- abs(range(shp_kenya$COORDS_Y)[2] - range(shp_kenya$COORDS_Y)[1]) # alternative way: abs(Reduce("-", range(shp_kenya$COORDS_X)))

# we add a custom nudge for some of the labels:
#   inspirattion: https://github.com/slowkow/ggrepel/issues/89
shp_kenya <- shp_kenya %>%
  mutate(
    nudge_x = case_when(
      County == "KISII" ~ -0.3 * x_range, 
      County == "BUSA" ~ -0.1 * x_range,
      County == "KISUMU" ~ -0.3 * x_range,
      County == "KERICHO" ~ -0.4 * x_range,
      County == "SAYA" ~ -0.1 * x_range,
      County == "ELGEYO/MARAKWET" ~ -0.4 * x_range,
      County == "NAIROBI CITY" ~ -0.1 * x_range,
      County == "THARAKA-NITHI" ~ 0.54 * x_range,
      County == "MOMBASA" ~ 0.05 * x_range,
      County == "MURANG'A" ~ -0.3 * x_range,
      County == "KIAMBU" ~ -0.16 * x_range,
      County == "TTRANS NZOIA" ~ -0.05 * x_range,
      County == "VIHIGA" ~ -0.2 * x_range,
      County == "EMBU" ~ 0.01 * x_range,
      TRUE ~ nudge_x
    ),
    nudge_y = case_when(
      County == "NAIROBI CITY" ~ -0.2 * y_range,
      County == "KIRINYAGA" ~ -0.4 * y_range,
      County == "MURANG'A" ~ -0.2 * y_range,
      County == "NYAMIRA" ~ -0.15 * y_range,
      County == "KIAMBU" ~ -0.19 * y_range,
      County == "ELGEYO/MARAKWET" ~ 0.15 * y_range,
      County == "TTRANS NZOIA" ~ -0.1 * y_range,
      County == "NANDI" ~ -0.005 * y_range,
      County == "KAKAMEGA" ~ -0.008 * y_range,
      County == "NYERI" ~ -0.008 * y_range,
      TRUE ~ nudge_y
    )
  )


# just because faster to inspect (View()) the metadata if don't need to load geometry:
metadata <- st_drop_geometry(shp_kenya) 


### --- PLOTTING STYLE

map_boundaries <- "darkgrey" # color of county borders
my_font <- "Fira Sans"

theme_set(theme_void() +
            theme(plot.background = element_rect(color = "white"),
                  legend.position = "right",
                  legend.text = element_text(family = my_font, size = 11),
                  legend.title = element_text(family = my_font, size = 11),
                  plot.title = element_text(family = my_font, size = 13, hjust = 0),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")))

# definition of custom palettes
quantile_palette <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac") # sequential palette with 2 hues, from colorbrewer

# Here we define ' as the big separator for the numbers in the legend (not necessary as be binarized by quantile)
# point <- format_format(big.mark = "'", decimal.mark = ".", scientific = FALSE)


### --- PLOT MAP

kenyan_pop_labels <- ggplot(shp_kenya) +
  geom_sf(aes(fill = Quant_Pop), color = map_boundaries, size = .2) +
  scale_fill_manual("Population ", values = quantile_palette, na.value = "grey") +
  geom_text_repel(
    mapping = aes(x = COORDS_X, y = COORDS_Y, label = County_title),
    nudge_x = shp_kenya$nudge_x,
    nudge_y = shp_kenya$nudge_y,
    size = 1.8,
    min.segment.length = 1,
    point.padding = 0, # Amount of padding around labeled point, as unit or number. Defaults to 0
    force = 0, # Force of repulsion between overlapping text labels. Defaults to 1.
    force_pull = 100, # Force of attraction between a text label and its corresponding data point. Defaults to 1.
    segment.color = "black", #"grey50"
    segment.size = 0.2
  ) +
  labs(
    title = "Kenya: Population by County" #,
    # caption = "Map by François Delavy - @f_delavy | Data by rKenyaCensus - github.com/Shelmith-Kariuki/rKenyaCensus"
  ) 

# Save as PNG:
ggsave(filename = "Kenya_Population_labelled.png", 
       plot = kenyan_pop_labels,
       width = 200,
       height = 200, 
       units = "mm",
       dpi = 600)

# mask_boundary_tmp <- st_transform(mask_boundary, 
#                                 "+proj=utm +zone=37 +south +a=6378249.145 +b=6356514.96582849 +units=m +no_defs")  

kenyan_pop <- ggplot(shp_kenya) +
  geom_sf(aes(fill = Quant_Pop), color = map_boundaries, size = .2) +
  scale_fill_manual("Population ", values = quantile_palette, na.value = "grey") +
  labs(
    title = "Kenya: Population by County" #,
  )
# + geom_sf(data = mask_boundary_tmp[9,], color = "red", size = 2)

# Save as PNG:
ggsave(filename = "Kenya_Population", 
       plot = kenyan_pop,
       width = 200,
       height = 200, 
       units = "mm",
       dpi = 600) 


