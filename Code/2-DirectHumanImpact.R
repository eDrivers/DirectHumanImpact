# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                    DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./data/rawData/censuspop.RData')
load('./Data/Grids/Data/egslCoast.RData')
load('./Data/Grids/Data/egslGrid.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 DRIVER LAYER
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select grid cells that are less than 2km from the coast and apply a 10km buffer
coast <- egslCoast %>%
         st_buffer(2000) %>% # Buffer around coast line
         st_intersects(egslGrid) %>% #Intersect with grid cells
         unlist() %>%
         egslGrid[., ] %>%
         st_buffer(10000)

# Intersect buffered coastal grids with dissemination areas to extract the sum of the populations
# Sum of the population should be proportional to the area of the dissemination area intersecting the buffer
# Preprocessing census data before intersections
censuspop <- st_simplify(censuspop, preserveTopology = T, 10) %>%
             st_buffer(10) %>%
             mutate(area = st_area(.)) %>%
             select(-DAUID, -PRNAME)

# Binary intersections
inter <- st_intersects(censuspop, coast)

# Zonal intersections with extracted population data
# Note that this is likely not the most efficient way to do it, but the analysis
# ran into memory issues, so I built it like this.
# The intersect function still ran in less than an hour
areaInter <- function(x, y) {
  x %>%
  st_intersection(coast[y, ]) %>%
  mutate(area2 = st_area(.)) %>%
  mutate(propArea = area2 / area) %>%
  mutate(population = Population..2016 * propArea) %>%
  select(ID, population) %>%
  st_set_geometry(NULL)
}

bindInter <- function(df, z) {
  df %>%
  rbind(z) %>%
  group_by(ID) %>%
  summarise(population = sum(population)) %>%
  mutate(population = as.numeric(population))
}

# Empty data.frame to store zonal intersects results
df <- data.frame(ID = character(),
                 population = numeric(),
                 stringsAsFactors = F)

# Zonal intersect analysis
for(i in 1:nrow(censuspop)) {
  zonalInter <- areaInter(censuspop[i, ], inter[[i]])
  df <- bindInter(df, zonalInter)
}

# Add to grid and change name
dirHumImpact <- left_join(egslGrid, df, by = 'ID') %>%
                rename(DirectHumanImpact = population)


# Select only cells with values
dirHumImpact <- dirHumImpact[!is.na(dirHumImpact$DirectHumanImpact), ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Change name for uniformity
DirectHumanImpact <- dirHumImpact

# Export object as .RData
save(DirectHumanImpact, file = './Data/Driver/DirectHumanImpact.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 VISUALIZE DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png('./Figures/DirectHumanImpact.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(dirHumImpact[, 'DirectHumanImpact'], border = 'transparent')
dev.off()
