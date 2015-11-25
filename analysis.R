setwd("Projects/Toronto/trees")

rm(list=ls(all=TRUE))

# Load the required libraries
library(rgdal)                # reading OGR files
library(rgeos)                # for converting shapefile into a dataframe
library(ggplot2)
gpclibPermit()                # need the gpclib package installed-- see notes for getting the source
library(ggmap)
library(dplyr)                # SQL-like operations on dataframe (join, filter, etc)
library(sp)
library(geosphere)            # for calculating the area of a SpatialPolygon given lons, lats
library(RColorBrewer)         # for setting color palettes

source("fxns.R")

# --------------- Read shapefiles
# -- Toronto neighborhoods
NBR_SHP <- spTransform(readOGR(dsn = "../data/neighbourhoods_planning_areas_wgs84", 
                               layer = "NEIGHBORHOODS_WGS84"),
                       CRS("+proj=longlat +datum=WGS84"))

# Transform into a dataframe
NBR_SHP@data$id <- as.character(sprintf("%03d", as.numeric(NBR_SHP@data$AREA_S_CD)))
NBR.DF <- fortify(NBR_SHP, region="AREA_S_CD")
NBR.DF <- inner_join(NBR.DF, NBR_SHP@data, by="id")    # neighborhood dataframe

# Toronto street trees
TREES_SHP <- spTransform(readOGR(dsn = "../data/street_tree_data_wgs84", 
                                 layer = "street_tree_general_data_wgs84"),
                         CRS("+proj=longlat +datum=WGS84"))

TREES.DF <- data.frame(common_name = TREES_SHP@data$COMMON_NAM,
                    bot_name =TREES_SHP@data$BOTANICAL_,
                    dia=TREES_SHP@data$DIAMETER_B,
                    long=TREES_SHP@coords[ , "coords.x1"],
                    lat=TREES_SHP@coords[ , "coords.x2"] )

APPLES = c("CRAB APPLE", "APPLE")
BERRIES = c("SERVICE-BERRY", "WEEPING WHITE MULBERRY", "BLACK (RED) MULBERRY", "HACK-BERRY")
APRICOTS = c("APRICOT")
CHERRIES = c("SHUBERT CHOKE CHERRY", "CHOKE CHERRY", "PIN CHERRY", "BLACK CHERRY")
SUMAC = c("SUMACH STAGHORN")
PEARS = c("CALLERY PEAR 'CHANTICLEER'")
PEACHES = c("PEACH")

EDIBLE_FRUITS = c(unlist(APPLES), unlist(BERRIES), unlist(APRICOTS), unlist(SUMAC), unlist(PEARS), unlist(PEACHES))
  
#--------------------------------------
# Top 10 tree types in Toronto (by count)
#--------------------------------------
count_type <- aggregate(TREES.DF, by=list(factor(TREES.DF$common_name), factor(TREES.DF$bot_name)), FUN=length)
count_type <- count_type[, 1:3]              # Columns 1,2,3 consists of common_name, bot_name, and count, respectively
names(count_type) = c("common_name", "bot_name", "count")
count_type$pct_total <- count_type$count/sum(count_type$count) * 100

count_type <- count_type[order(count_type$count, decreasing=TRUE), ]    # sort data frame by count from highest to lowest

# assign level to common_name so that the plot is ordered
count_type$common_name <- factor(count_type$common_name, 
                                 level=count_type[order(count_type$count, decreasing=FALSE), "common_name"])
# Make a bar plot
p <- ggplot(data=count_type[1:10, ], aes(x=factor(common_name), y=pct_total, fill="red")) + # fill argument doesn't work? 
      geom_bar(stat="identity") + coord_flip() +
      labs(title="Top 10 Kinds of Toronto Street Trees",
           y="% out of all street trees") +
      theme(axis.title.y=element_blank(),
            plot.title = element_text(size = 18,face="bold"), 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold")) +
      guides(fill=FALSE)                 # remove legend as it is redundant

png("top10_trees.png", width=8.5, height=6, units="in",res=300)
p
dev.off()


# ------------------------------------------------ 
# A chorophleth map of tree density by neighborhood
# ------------------------------------------------
# get the tree density in each neigborhood (function takes some time to run). Could change this once sure
# area is computed properly
len_nbr <- length(unique(NBR.DF$AREA_NAME))
trees_nbr_df <- data.frame(nbrhood=unique(NBR.DF$AREA_NAME), 
                           count=0, 
                           area_km2=0, 
                           density=0)
# check if area is calculated properly...
for (nbr in trees_nbr_df$nbrhood) {
  dens <- treeDensity_In_Neighborhood(nbr)
  trees_nbr_df[trees_nbr_df$nbrhood == nbr, "count"] = dens$count
  trees_nbr_df[trees_nbr_df$nbrhood == nbr, "area_km2"] = dens$area
}
trees_nbr_df$density = trees_nbr_df$count/trees_nbr_df$area_km2

# Set Polygon colors
# make bins of tree densities
binLims = c(300, 600, 900, 1200, 1500, 1800)
# assign trees_nbr_df$density to color bins
binColors = brewer.pal(n = 7, name = "Greens")
binLabels = c("<300", "300-599", "600-899", "900-1199", "1200-1499", "1500-1799", ">1800")
assign_bin <- function(x) {
  if (x < binLims[1]){bin = binLabels[1]} 
  else {if (x >= binLims[1] & x < binLims[2]){bin = binLabels[2]}
  else {if (x >= binLims[2] & x < binLims[3]){bin = binLabels[3]}
  else {if (x >= binLims[3] & x < binLims[4]){bin = binLabels[4]}
  else {if (x >= binLims[4] & x < binLims[5]){bin = binLabels[5]}
  else {if (x >= binLims[5] & x < binLims[6]){bin = binLabels[6]}
  else {bin = binLabels[7]}}}}}}
  bin
}  
# make a copy of the df for joining with the NBR.DF
temp_df <- data.frame(AREA_NAME = trees_nbr_df$nbrhood,
                   tree_density = factor(sapply(trees_nbr_df$density, assign_bin),
                                         levels=binLabels))

if (any(names(NBR.DF) == "tree_density")) {
  NBR.DF <- NBR.DF[ , -which(names(NBR.DF) == "tree_density")]
}
NBR.DF <- inner_join(NBR.DF, temp_df, by="AREA_NAME")

rm(temp_df)                                            # then remove temp_df

# the map
to_geo <- geocode("Toronto")
gmap <- get_map(location=c(lon=to_geo[[1]], lat=to_geo[[2]]+0.02),
                source="google", maptype="roadmap", zoom=11)

p <- ggmap(gmap) + 
  geom_polygon(data=NBR.DF, 
               aes(x=long, y=lat,
                   fill=tree_density, 
                   group=group),
               color = "white", alpha=0.7, size = 0.1) +
  scale_fill_manual(values = c("<300" = binColors[1],
                               "300-599" = binColors[2],
                               "600-899" = binColors[3],
                               "900-1199" = binColors[4],
                               "1200-1499" = binColors[5],
                               "1500-1799" = binColors[6],
                               ">1800" = binColors[7]
                              )) + 
  theme(axis.ticks.x =element_blank(), axis.ticks.y =element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 16,face="bold")) +
  labs(title = "Density of Street Trees in Toronto",
       fill="trees per sq. km")
  
png("tree_density.png", width=8, height=9, units="in", res=300)
p
dev.off()

#--- The 3 greenest and 3 least green neighborhoods (note that the dataset only includes street trees)
trees_nbr_df[order(trees_nbr_df$density, decreasing=TRUE)[1:3], c("nbrhood", "density")]           # Top 3 neighborhoods
#nbrhood  density
#15      Kingsway South (15) 1906.214
#47  Don Valley Village (47) 1889.185
#101 Forest Hill South (101) 1837.375

trees_nbr_df[order(trees_nbr_df$density, decreasing=FALSE)[1:3], c("nbrhood", "density")]          # Bottom 3 neighborhoods
#nbrhood  density
#55                   Thorncliffe Park (55) 137.3833
#131                            Rouge (131) 200.3284
#77  Waterfront Communities-The Island (77) 240.7976

#--------------------------
# Edible trees - plot all edible trees in Toronto
#--------------------------
# Peach and apricot trees
map = get_map(location=c(lon=to_geo[[1]]-0.02, lat=to_geo[[2]]+0.02),
              source="google", maptype="roadmap", zoom=11)

p = ggmap(map) +
  geom_point(data=TREES.DF[TREES.DF$common_name %in% PEACHES |
                           TREES.DF$common_name %in% APRICOTS, ], 
             aes(x=long, y=lat, color=common_name), size=1) +
  theme(axis.ticks.x =element_blank(), axis.ticks.y =element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 16,face="bold")) +
  labs(title = "Locations of Peach and Apricot Trees",
       color="Fruit type")

png("peach_apricot_trees.png", width=8, height=9, units="in", res=300)
p
dev.off()


# Show all edible trees within x m radius of a specified address in Toronto



# Show all edible trees in a neighborhood
