setwd("~/Python")
collibrary(maps)
library(maptools)
library(sp)
library(lattice)
library(latticeExtra)
library(colorspace)
library(RColorBrewer)
library(ggmap)

data<-read.csv("output_file.csv")

area <- readShapePoly("~/Python/PER_adm/PER_adm1.shp")
colors<-brewer.pal(9,"BuGn")

mapImage <- get_map(location = c(lon = -76, lat = -10),
                    color = "color",
                    source = "osm",
                    # maptype = "terrain",
                    zoom = 6)

area.points <- fortify(area)

ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

