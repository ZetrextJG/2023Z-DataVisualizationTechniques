library(sf)
library(mapview)
library(dplyr)
library(leaflet)

# Optain data
# download.file("http://bit.ly/R-spatial-data", "R-spatial-data.zip")
# unzip("R-spatial-data.zip", exdir = "data")

# Load shape file
philly_map_raw <- read_sf("./data/Philly/PhillyTotalPopHHinc.shp", quiet = TRUE)
philly_map <- philly_map_raw %>%
  mutate(
    SECTOR_ID = GEOID10 %/% 100,
    .keep = "all"
  )

# Load data set with GEOID
education_dataset <- read.csv("./data/PhillyEducation.csv")
philly_ed_data <- education_dataset %>%
  mutate(
    SECTOR_ID = GEOID %/% 100,
    .keep = "all"
  ) %>%
  group_by(SECTOR_ID) %>%
  mutate(
    fem_higher_ed = fem_bachelor + fem_doctorate,
    male_higher_ed = male_bachelor + male_doctorate,
    fem_pop = fem_ovr_25,
    male_pop = male_ovr_25,
    .keep = "unused"
  ) %>%
  select(SECTOR_ID, fem_higher_ed, male_higher_ed, fem_pop, male_pop) %>%
  mutate(
    fem_higher_proc = round((fem_higher_ed / fem_pop) * 100, 2),
    male_higher_proc = round((male_higher_ed / male_pop) * 100, 2),
    fem2male_comp = (fem_higher_ed - male_higher_ed) / (fem_higher_ed + male_higher_ed),
    display = paste0(
      "Women: ", fem_higher_proc, "% |\n",
      "Men: ", male_higher_proc, "%"
    ),
    .keep = "all"
  )

data_map <- inner_join(philly_map, philly_ed_data, by = c("SECTOR_ID" = "SECTOR_ID"))

color_pal <- colorRampPalette(c("blue", "red"))

mapview(data_map,
        zcol = "fem2male_comp",
        label = "display",
        layer.name = paste(
          "Difference in education by sex.",
          "red  = more women",
          "blue = more men",
          sep = "<br>"
        ),
        col.regions = color_pal(364),
        legend = TRUE
) 

# Additinal functionality
# install.packages('leaflet.extras2')

men_map <- mapview(data_map,
        zcol = "male_higher_proc",
        label = "display",
        layer.name = "Percentage of men with higher education",
        legend = TRUE
)

women_map <- mapview(data_map,
        zcol = "fem_higher_proc",
        label = "display",
        layer.name = "Percentage of women with higher education",
        legend = TRUE
) 

men_map | women_map
