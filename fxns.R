# --------- plotting a kind of trees inside a neighborhood
plotTreesInNeighborhood <- function(neighborhood_name, kind_of_tree='apple', pointColor="purple", polyColor="green") 
{  
  
  # Get the shapefile for the neighborhood and convert to dataframe
  sub.nbr_df = inner_join(NBR.DF, 
                          NBR_SHP@data[NBR_SHP@data$AREA_NAME == neighborhood_name, ], 
                          by="id")
  
  sub.nbr_shp = NBR_SHP[NBR_SHP@data$AREA_NAME == neighborhood_name, ]
  
  # Get the centroid
  sub.nbr_centroid = coordinates(sub.nbr_shp)
  
  # Find trees within the neighborhood : use over() 
  if (kind_of_tree == 'apple') {
    sub.trees_shp = TREES_SHP[grepl('.*APPLE.*', TREES_SHP@data$COMMON_NAM), ] 
  } else {
    sub.trees_shp = TREES_SHP[grepl('.*PEAR.*', TREES_SHP@data$COMMON_NAM), ] 
  }
  
  inside.nbr = !is.na(over(sub.trees_shp, as(sub.nbr_shp, "SpatialPolygons")))
  
  # Convert trees inside the polygon into a dataframe
  trees.inside = sub.trees_shp[inside.nbr, ] 
  
  trees.inside_df =  data.frame(address=trees.inside@data$ADDRESS_FU,
                                common_name = trees.inside@data$COMMON_NAM,
                                bot_name =trees.inside@data$BOTANICAL_,
                                dia=trees.inside@data$DIAMETER_B,
                                long=trees.inside@coords[ , "coords.x1"],
                                lat=trees.inside@coords[ , "coords.x2"] )
  
  map = get_map(location= c(lon=sub.nbr_centroid[[1]], lat= sub.nbr_centroid[[2]]),
                source="google", maptype="roadmap", zoom=14)
  
  p = ggmap(map) +
    geom_polygon(data=sub.nbr_df, 
                 aes(x=long, y=lat, group=group),
                 fill="red", color = polyColor, alpha=0, size=1) +
    geom_point(data=trees.inside_df, 
               aes(x=long, y=lat), color=pointColor, size=1)    
}

# --------- Count all the trees in a specified neighborhood
treeDensity_In_Neighborhood <- function(neighborhood_name) 
{ 
  # The shapefile defining the neighborhood (consisting of a polygon)
  sub.nbr_shp = NBR_SHP[NBR_SHP@data$AREA_NAME == neighborhood_name, ]
  
  # Count of trees in the neighborhood
  count = sum(!is.na(over(TREES_SHP, as(sub.nbr_shp, "SpatialPolygons"))))
  
  # Area in km^2
  area = areaPolygon(as(sub.nbr_shp, "SpatialPolygons"))/1e6
  
  data.frame(count=count, area=area)
}


  