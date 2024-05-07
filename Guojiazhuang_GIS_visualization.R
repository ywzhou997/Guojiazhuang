#install.packages("ggbiplot")
#install.packages("maptools")
#install.packages("spatstat")
#install.packages("factoextra")


# loading library
library(readxl)
library(xlsx)
library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(tmap)
library(devtools)
library(showtext)
library(ggbiplot)
library(DAAG)
library(plyr)
library(stringr)
library(grid)
library(gridExtra)
library(randomcoloR)
library(maptools)
library(spatstat)
library(factoextra)
showtext_auto()


#Set up
getwd()
#setwd("../Guojiazhuang")
prj_dd <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


Guojiazhuang_data <- read_excel("./郭家莊墓葬形制44.xlsx", col_types = c(rep("text", 44)))
head(Guojiazhuang_data)
pottery_combination <- read_excel("./pottery_combinations.xlsx", col_types = c(rep("text", 105)))
Guojiazhuang_burials <- st_read("./Guojiazhuang_burials/Guojiazhuang_burials.shp")
Guojiazhuang_boundary <- st_read("./Guojiazhuang_boundary/Guojiazhuang_boundary.shp")
Guojiazhuang_outline <- st_read("./Guojiazhuang_outline/Guojiazhuang_outline.shp")

# taking out unexcavated tombs M122, 123
Guojiazhuang_burials <-
  Guojiazhuang_burials %>% subset(!(
    Guojiazhuang_burials$Num == "122" |
      Guojiazhuang_burials$Num == "123"
  ))
head(Guojiazhuang_burials)



#combine data
Guojiazhuang_burials[,c(5:(2+ncol(Guojiazhuang_data)))] <- NA
colnames(Guojiazhuang_burials)[5:ncol(Guojiazhuang_burials)] <- colnames(Guojiazhuang_data)[3:ncol(Guojiazhuang_data)]
colnames(Guojiazhuang_burials)
for(i in 1:nrow(Guojiazhuang_burials)) {
  temp <- Guojiazhuang_data[Guojiazhuang_data$num == Guojiazhuang_burials$Num[i],-c(1, 2)]
  Guojiazhuang_burials[i,c(5:ncol(Guojiazhuang_burials))] <- temp
}
for (i in 1:nrow(Guojiazhuang_burials)) {
  Guojiazhuang_burials$Report_zone_simp[i] <-
    strsplit(Guojiazhuang_burials$Report_zone[i], "")[[1]][1]
}

Guojiazhuang_burials_qgis <- Guojiazhuang_burials
for(i in 1:nrow(Guojiazhuang_burials_qgis)){
  if(!is.na(Guojiazhuang_burials_qgis$period[i] == "二")){
    if(Guojiazhuang_burials_qgis$period[i] == "二"){
      Guojiazhuang_burials_qgis$period[i] <- "II"
    } else if(Guojiazhuang_burials_qgis$period[i] == "三") {
      Guojiazhuang_burials_qgis$period[i] <- "III"
    } else if(Guojiazhuang_burials_qgis$period[i] == "四早") {
      Guojiazhuang_burials_qgis$period[i] <- "IVa"
    } else if(Guojiazhuang_burials_qgis$period[i] == "四晚") {
      Guojiazhuang_burials_qgis$period[i] <- "IVb"
    }
  }
}
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)], "./Guojiazhuang_info.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "II"), "./Guojiazhuang_infoII.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "II" | period == "III" ), "./Guojiazhuang_infoII_III.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "III"), "./Guojiazhuang_infoIII.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "IVa"), "./Guojiazhuang_infoIVa.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "II" | period == "III" | period == "IVa"), "./Guojiazhuang_infoII_III_IVa.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "IVb"), "./Guojiazhuang_infoIVb.shp")
st_write(Guojiazhuang_burials_qgis[,-c(7:24, 44)] %>% filter(period == "II" | period == "III" | period == "IVa" | period == "IVb"), "./Guojiazhuang_infoII_III_IVab.shp")


#===Map visualization====
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, line = "white", lwd = 1.3) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, col = "white", lwd = 1) +
  tm_shape(Guojiazhuang_burials) + tm_polygons(col = "white") +
  #tm_shape(Guojiazhuang_burials %>% filter(ritual_bronze == "TRUE")) + tm_dots(col = "black", size = 0.1) +
  # tm_shape(Guojiazhuang_burials %>% filter(ritual_bronze == "TRUE") %>% filter(Report_zone_simp == "北")) + 
  # tm_dots(col = "red", size = 0.1) + tm_text(text = "Id", size = 0.6, ymod = -0.3, xmod = 0.3, col = "red") +
  # tm_shape(Guojiazhuang_burials %>% filter(ritual_bronze == "TRUE") %>% filter(Report_zone_simp == "中")) + 
  # tm_dots(col = "green", size = 0.1) + tm_text(text = "Id", size = 0.6, ymod = -0.3, xmod = -0.3, col = "darkgreen") +
  # tm_shape(Guojiazhuang_burials %>% filter(ritual_bronze == "TRUE") %>% filter(Report_zone_simp == "南")) + 
  # tm_dots(col = "purple", size = 0.1) + tm_text(text = "Id", size = 0.6, xmod = 0.5, col = "purple") +
  # tm_shape(Guojiazhuang_burials %>% filter(is.na(burial_good_bronze))) + tm_dots(col = "red", alpha = 0.5, size = 0.05) +
  # tm_shape(Guojiazhuang_burials %>% filter(grepl("戈", burial_good_bronze) | grepl("镞", burial_good_bronze))) + tm_dots(col = "blue", alpha = 0.5, size = 0.03) +
  # tm_shape(Guojiazhuang_burials %>% filter(ritual_bronze == "TRUE")) + tm_dots(col = "orange", alpha = 0.5, size = 0.01)
  tm_shape(Guojiazhuang_burials %>% filter(grepl("觚", burial_good_pottery) & !grepl("爵", burial_good_pottery))) + tm_dots(col = "blue", alpha = 0.5, size = 0.03) +
  tm_shape(Guojiazhuang_burials %>% filter(!grepl("觚", burial_good_pottery) & grepl("爵", burial_good_pottery))) + tm_dots(col = "blue", alpha = 0.5, size = 0.03) +
  #tm_shape(Guojiazhuang_burials %>% filter(ritual_bronze == "TRUE")) + tm_dots(col = "orange", alpha = 0.5, size = 0.01)
  tm_scale_bar(position = c("center", "top"), breaks = c(0, 20, 40)) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_add_legend(labels = c("Northern", "Middle", "Southern", "Excluded"), col = c("red", "green", "purple", "black")) +
  tm_layout(legend.position = c("left","bottom"), legend.text.size = 0.6)

#==General===
#period two
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0,  lwd = 1.3) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(col = "red", lwd = 2) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

#period three
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5,  lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(col = "red", lwd = 2) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

#period four early
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四早")) + tm_borders(col = "red", lwd = 2) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

#period four late
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2)+
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四早")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四晚")) + tm_borders(col = "red", lwd = 2) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")


#period two
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(col = "red", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二") %>% filter(burial_dog == TRUE)) + tm_polygons(col = "darkgreen", lwd = 0) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

#period three
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二") %>% filter(waist_pit == TRUE)) + tm_polygons(col = "black", lwd = 0)+
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(col = "red", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三") %>% filter(waist_pit == TRUE)) + tm_polygons(col = "black", lwd = 0)+
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

#period four early
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二") %>% filter(waist_pit == TRUE)) + tm_polygons(col = "black", lwd = 0)+
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三") %>% filter(waist_pit== TRUE)) + tm_polygons(col = "black", lwd = 0) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四早")) + tm_borders(col = "red", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四早") %>% filter(waist_pit == TRUE)) + tm_polygons(col = "black", lwd = 0)+
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

#period four late
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2)+
  tm_shape(Guojiazhuang_burials %>% filter(period == "二") %>% filter(waist_pit== TRUE)) + tm_polygons(col = "black", lwd = 0) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三") %>% filter(waist_pit== TRUE)) + tm_polygons(col = "black", lwd = 0) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四早")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四早") %>% filter(waist_pit== TRUE)) + tm_polygons(col = "black", lwd = 0) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四晚")) + tm_borders(col = "red", lwd = 2) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(type = "4star", size = 2, position = c("right", "top")) +
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")


# burial goods bronze
tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(waist_pit== TRUE) %>% filter(burial_dog == TRUE)) + tm_polygons(col = "period", lwd = 0.5) +
  #tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_symbols(shape = "burial_style", size = 0.5) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二")) + tm_borders(alpha = 0.5, col = "grey", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "二") %>% filter(grepl("陶觚", burial_good_pottery)) %>% filter(grepl("陶爵", burial_good_pottery)) %>% filter(burial_dog = TRUE)) + tm_polygons(col = "blue", lwd = 0.5) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "三") %>% filter(grepl("陶觚", burial_good_pottery)) %>% filter(grepl("陶爵", burial_good_pottery)) %>% filter(burial_dog = TRUE)) + tm_polygons(col = "pink", lwd = 0.5) +
  #tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_symbols(shape = "burial_style", size = 0.5) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")


tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四晚") %>% filter(burial_jade == TRUE)) + tm_borders(col = "blue", lwd = 2) +
  tm_shape(Guojiazhuang_burials %>% filter(period == "四晚") %>% filter(ritual_bronze == TRUE)) + tm_symbols(shape = 21, size = 0.05) +
  #tm_shape(Guojiazhuang_burials %>% filter(period == "三")) + tm_symbols(shape = "burial_style", size = 0.5) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials %>% filter(Num == "225" | Num == "226" | Num == "228" | Num == "229" | Num == "230" | Num == "232" | Num == "231")) + tm_borders(col = "blue", lwd = 2) +
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_layout(main.title = "Guojiazhuang cemetery distribution", title.size = 1, main.title.position = "center")


#buffer
Guojiazhuang_burials_II <- Guojiazhuang_burials %>% filter(period == "二")
Guojiazhuang_burials_III <- Guojiazhuang_burials %>% filter(period == "三")
Guojiazhuang_burials_IVa <- Guojiazhuang_burials %>% filter(period == "四早")
Guojiazhuang_burials_IVb<- Guojiazhuang_burials %>% filter(period == "四晚")
Guojiazhuang_burials_II_1m <- st_buffer(Guojiazhuang_burials_II, dist = 1.15)
Guojiazhuang_burials_III_1m <- st_buffer(Guojiazhuang_burials_III, dist = 1.15)
Guojiazhuang_burials_IVa_1m <- st_buffer(Guojiazhuang_burials_IVa, dist = 1.15)
Guojiazhuang_burials_IVb_1m <- st_buffer(Guojiazhuang_burials_IVb, dist = 1.15)
Guojiazhuang_burials_1m <- st_buffer(Guojiazhuang_burials, dist = 2)

plot(Guojiazhuang_burials)

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials) + tm_polygons(col = "black")+
  tm_shape(Guojiazhuang_burials_1m) + tm_polygons(col = "purple", alpha = .3, lwd = 0)

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials_II) + tm_polygons(col = "black")+
  tm_shape(Guojiazhuang_burials_II_1m) + tm_polygons(col = "purple", alpha = .3, lwd = 0)

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials_II) + tm_borders(col = "grey")+
  tm_shape(Guojiazhuang_burials_III) + tm_polygons(col = "black")+
  tm_shape(Guojiazhuang_burials_III_1m) + tm_polygons(col = "purple", alpha = .3, lwd = 0)

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_burials_II) + tm_borders(col = "grey")+
  tm_shape(Guojiazhuang_burials_III) + tm_borders(col = "grey")+
  tm_shape(Guojiazhuang_burials_IVa) + tm_polygons(col = "black")+
  tm_shape(Guojiazhuang_burials_IVa_1m) + tm_polygons(col = "purple", alpha = .3, lwd = 0)

tm_shape(Guojiazhuang_boundary, unit = "m") + tm_polygons(alpha = 0, lwd = 1.2) +
  tm_shape(Guojiazhuang_outline) + tm_polygons(alpha = 0.5, lwd = 1) +
  tm_shape(Guojiazhuang_centroid) + tm_dots(col = "grey") +
  tm_shape(Guojiazhuang_burials_III) + tm_borders(col = "grey")+
  tm_shape(Guojiazhuang_burials_IVa) + tm_borders(col = "grey")+
  tm_shape(Guojiazhuang_burials_IVb) + tm_polygons(col = "black")+
  tm_shape(Guojiazhuang_burials_IVb_1m) + tm_polygons(col = "purple", alpha = .3, lwd = 0)


Guojiazhuang_centroid <- st_read("./QGIS/Centroid_III.shp")
Guojiazhuang_centroid <- st_read("./QGIS/Centroid_all.shp")
nrow(Guojiazhuang_centroid)
points=as.ppp(Guojiazhuang_centroid)
K <- Kest(points, correction = "Ripley")
kf.env <- envelope(points, Kest, correction = "Ripley")
plot(kf.env)



# calculating elbow point for k means analysis from QGIS
#k-means clustering

centriod <- st_read("./QGIS/Centroid_IVb.shp")
coord <- st_coordinates(centriod)
coordm <- apply(coord, 2, mean)
coords <- apply(coord, 2, sd)
coord <- scale(coord, coordm, coords)
fviz_nbclust(coord, kmeans, method = "wss") + labs(subtitle = "Elbow method (Period II & III & IVa & IVb)")
