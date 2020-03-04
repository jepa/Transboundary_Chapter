library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(rnaturalearth)
library(sf)
library(wesanderson)
source("./Functions/get_data_fx.R")

#### Data Source ###
# Data shared by Mark McMillan thanks to Catherine Micielsens from PSC
# Reference info
# If the 2018 report is not yet published it might be better to referrer
# the Chinook Technical Committee of the Pacific Salmon Commission as the source.

#________________________________________________________________________________#

# Manually create fisheries labels for fixing data

Fishery_Set <- c(
  rep("SEK",3),
  rep("NBC",2),
  rep("WCVI",2),
  rep("NBC&CB",3),
  rep("SBC",3),
  rep("NF",2),
  rep("SF",2),
  rep("WAK",1),
  rep("PUG",2),
  rep("SEK",3),
  rep("CAN",2),
  rep("SUS",3),
  "Stray",
  "Esc"
)

# Create tabel relation
Fishery_to_Country <- tibble(
  Fishery = c("SEK","NBC","WCVI","NBC&CB","SBC","NF","SF","WAK","PUG","CAN","SUS","Stray","Esc"),
  Fishing_Country = c("US Alaska",rep("Canada",4),rep("US Contiguous States",2),rep("US Contiguous States",2),"Canada","US Contiguous States","Stray","Esc")
)

# Load Fishery Acronyms

Acronyms <- read_excel("Data/2018 Chinook Total Mortality Distribution.xlsx",
                       sheet = "Fisheries", col_names = TRUE) %>% 
  arrange(Region) %>% 
  mutate(Sheet = paste(StockAcronym,"total mort")) %>% 
  mutate(Country = c(rep("US Contiguous States",22),rep("Canada",4),rep("US Contiguous States",18),rep("US Alaska",3),rep("Transboundary",2),"Canada",rep("US Contiguous States",4))
  )



# Run function to get all Data
suppressMessages(
  Chinook_Data <- bind_rows(
    lapply(Acronyms$Sheet,get_chinook_data)
  )
) 


Total_Region_Catch <- Chinook_Data %>% 
  group_by(Region) %>% 
  summarise(Total_Reg_Catch = sum(Catch_Country,na.rm=T)) %>% 
  filter(!is.na(Region))

Regional_Coord <- tibble(
  Region = unique(Total_Region_Catch$Region),
  Lon = c(122,123,132.9,137,122,145,140,128,128),
  Lat = c(43.5,53,50.4,54,48,57,48,48,43.5)
)

Chinook_Per_Region <- Chinook_Data %>% 
  group_by(Fishing_Country,Region) %>% 
  summarise(Total_Country_Catch = sum(Catch_Country,na.rm=T)) %>% 
  left_join(Total_Region_Catch, 
            by = "Region") %>% 
  mutate(Region_per = (Total_Country_Catch/Total_Reg_Catch)*100) %>% 
  filter(!is.na(Region)) %>% 
  left_join(Regional_Coord)

# Plot Pie charts
# ggplot(Chinook_Per_Region) +
#   geom_bar(
#     aes(
#       x = "",
#       y = Region_per,
#       fill = Fishing_Country
#     ),
#     stat ="identity"
#   ) +
#   coord_polar("y", start=0) +
#   facet_wrap(~Region)


#### The Map

### Give coord for map

Test <- Chinook_Per_Region %>% 
  select(Region,Fishing_Country,Region_per) %>% 
  spread(Fishing_Country,Region_per) %>% 
  left_join(Regional_Coord) %>% 
  mutate(Lon = Lon*-1,
         radius = 3)


#### Option C
# Map Dimenssions
xmin=-121
xmax=-150
ymin=33
ymax=63

world = map_data("world", resolution=0)

p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), 
           fill=wes_palette(name = "IsleofDogs2")[1],
           # fill = "grey90",
           color="black",
           size = 0.1) +
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(
    panel.grid = element_blank(),
    # panel.grid.major = element_line(color = "gray90",
                                    # linetype = "dashed",
                                    # size = 0.5),
    # panel.grid.minor = element_line(color = "gray90",
    #                                 linetype = "dashed",
    #                                 size = 0.5),
    # panel.grid.minor = element_blank(),
    # panel.background = element_blank()
      panel.background = element_rect(fill = wes_palette(name = "Darjeeling2")[4])
  )
  
# Make magic for Pie not to be distorted
# https://stackoverflow.com/questions/51398344/r-pie-charts-distorted-when-adding-to-projected-map-using-ggplot

pie.list <- Test %>% 
  tidyr::gather(type, value, -Lon, -Lat, -radius, -Region) %>%
  tidyr::nest(type, value) %>%
  # make a pie chart from each row, & convert to grob
  mutate(pie.grob = purrr::map(data,
                               function(d) ggplotGrob(ggplot(d, 
                                                             aes(x = 1, y = value, fill = type)) +
                                                        geom_col(color = "black",
                                                                 show.legend = FALSE) +
                                                        coord_polar(theta = "y") +
                                                        scale_fill_manual("Fishing Entity",
                                                                          values = c(wes_palette(name = "Darjeeling1")[2:5]
                                                                          )
                                                        ) +
                                                        theme_void()))) %>%
  
  # convert each grob to an annotation_custom layer. I've also adjusted the radius
  # value to a reasonable size (based on my screen resolutions).
  rowwise() %>%
  mutate(subgrob = list(annotation_custom(grob = pie.grob,
                                          xmin = Lon - radius, xmax = Lon + radius,
                                          ymin = Lat - radius, ymax = Lat + radius))
         )

### Final Map

p + 
  geom_tile(data = Test %>% tidyr::gather(type, value, -Lon, -Lat, -radius,-Region),
            aes(x = Lon, y = Lat, fill = type), 
            color = "black", width = 0.01, height = 0.01, 
            inherit.aes = FALSE) +
  
  pie.list$subgrob +
  ggrepel::geom_text_repel(data = Test,
                            aes(
                              x = Lon,
                              y = Lat,
                              label = Region),
                            force = 5,
                            size = 3,
                            nudge_x = 0,
                            nudge_y = 2.1,
                            segment.color	= NA
  ) +
  scale_fill_manual("Fishing Entity",
                    values = c(wes_palette(name = "Darjeeling1")[2:5]
                    )
  ) +
  scale_x_continuous("Longitude",
                     breaks = seq(xmax,-120,5)
                     ) +
  scale_y_continuous("Latitude",
                     breaks = seq(30,65,5)
  ) +
  annotate("text", 
           x =-140,
           y = 36.3,
           label = "Sharing Chinook Salmon", 
           color="black", 
           size=6, 
           angle=0, 
           fontface="italic",
           hjust=0) +
  annotate("text", 
           x =-132,
           y = 34.8,
           label = "Regional Chinook Salmon Captures per\nFishing Entity Between 2009 and 2017", 
           color="black", 
           size=3, 
           angle=0, 
           # fontface="italic",
           hjust=0.5) +
  theme(
    legend.position = c(0.18,0.09),
    legend.background = element_rect(fill = wes_palette(name = "Darjeeling2")[4])
  )


ggsave("Col_Chinook_Map.png",
       plot = last_plot(),
       width = 6,
       height = 8,
       units = "in")

### Map for migration

xmin=-111
xmax=-160
ymin=45
ymax=63

states <- map_data("state")

ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), 
           fill=wes_palette(name = "IsleofDogs2")[1],
           # fill = "grey90",
           color="black",
           size = 0.1) +
  geom_polygon(data = states,
               aes(x = long, y = lat,
                   group = group), 
               fill=wes_palette(name = "IsleofDogs2")[1],
               color = "black",
               # fill = "grey90",
               size = 0.1) + 
  coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme(
    panel.grid = element_blank(),
    # panel.grid.major = element_line(color = "gray90",
    # linetype = "dashed",
    # size = 0.5),
    # panel.grid.minor = element_line(color = "gray90",
    #                                 linetype = "dashed",
    #                                 size = 0.5),
    # panel.grid.minor = element_blank(),
    # panel.background = element_blank()
    panel.background = element_rect(fill = wes_palette(name = "Darjeeling2")[4])
  )

ggsave("col_migration_map.png",
       plot = last_plot(),
       width = 8,
       height = 6,
       units = "in")
