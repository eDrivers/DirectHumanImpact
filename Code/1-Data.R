# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize direct human impact is the canadian population
# census data from Statistics Canada.
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './Data/RawData'

# Will have to add code to access the data on the Statistics Canada website.
# For now, I'm using the data downloaded manually from the website.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------- #
# Dissemination areas #
# ------------------- #

# Dissemination Area Boundary File, 2016 Census. Statistics Canada Catalogue no. 92-169-X.
# File name
fileName <- dir(output, pattern = '.zip')

# Unzip kmz file
unzip(zipfile = paste0(output, '/', fileName),
      exdir = output)

# Identify newly extracted files
fileName <- dir(output, pattern = 'shp$', full.names = T)

# Import shapefile and select relevant columns
censusdissem <- st_read(fileName) %>%
                dplyr::select(DAUID, PRNAME)

# --------------- #
# Population data #
# --------------- #
# Statistics Canada. 2017. Population and Dwelling Count Highlight Tables. 2016 Census.
pop <- read.csv('./data/rawdata/T1901EN.csv', stringsAsFactors = F) %>%
       dplyr::select(Geographic.code,Population..2016) %>%
       dplyr::mutate(Geographic.code = as.integer(Geographic.code))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   SELECT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Only coastal cells are affected by this drivers
# We therefore select only coastal cells within 2km of the coast
# We will be using a 10km buffer later on for the analyses to extract total
# population affecting grid cells

# Load egsl area object
load('./Data/Grids/Data/egslSimple.RData')

# Select census dissemination areas < 15km from the coast
censusdissem <- censusdissem %>%
                filter(PRNAME %in% c("New Brunswick / Nouveau-Brunswick",
                                     "Quebec / Québec",
                                     "Nova Scotia / Nouvelle-Écosse",
                                     "Prince Edward Island / Île-du-Prince-Édouard",
                                     "Newfoundland and Labrador / Terre-Neuve-et-Labrador")) %>%
                st_transform(st_crs(egslSimple)) %>%
                filter(lengths(st_intersects(.,st_buffer(st_union(st_convex_hull(egslSimple)),dist=15000)))>0) %>% #coarse filter
                filter(lengths(st_intersects(.,st_union(st_buffer(egslSimple,dist=15000))))>0) %>%                 #fine filter
                mutate(DAUID = as.integer(as.character(DAUID))) # for the join with population below


# Join population data
censuspop <- left_join(censusdissem, pop, by=c("DAUID"="Geographic.code"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(censuspop, file = './data/rawData/censuspop.RData')
