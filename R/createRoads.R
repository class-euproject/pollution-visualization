library(sf)
#cam_x <- st_linestring(rbind(c(44.658583, 10.930972),c(44.658333, 10.930667), c(44.658059, 10.930745)))
#cam_x <- rbind(c(44.657887, 10.930954),c(44.658059, 10.930745),c(44.657869, 10.930283))
cam_20937 <- st_linestring(rbind(rev(c(44.655691, 10.934219)), rev(c(44.656313, 10.933339))))
cam_20939 <- st_linestring(rbind(rev(c(44.655271, 10.935525)), rev(c(44.655271, 10.934825))))
aux_1 <- st_linestring(rbind(rev(c(44.655749, 10.934154)), rev(c(44.655360, 10.934511))))
aux_2 <- st_linestring(rbind(rev(c(44.655362, 10.934750)), rev(c(44.655360, 10.934511))))
aux_3 <- st_linestring(rbind(rev(c(44.655154, 10.934567)), rev(c(44.655360, 10.934511))))

cam_20936 <- st_multilinestring(list(aux_1, aux_2, aux_3))


cam_634 <- st_linestring(rbind(rev(c( 44.656866, 10.929196)) , rev(c(44.656511, 10.929003)), 
                                    rev(c(44.656290, 10.929142)), rev(c(44.656168, 10.928868)), 
                                    rev(c(44.656290, 10.928594)), rev(c(44.656511, 10.929003))))
aux_1 <- st_linestring(rbind(rev(c(44.656044, 10.928245)), rev(c(44.656254, 10.928428))))
aux_2 <- st_linestring(rbind(rev(c(44.656222, 10.928120)), rev(c(44.656254, 10.928428))))
aux_3 <- st_linestring(rbind(rev(c(44.656222, 10.928120)),  rev(c(44.656290, 10.928594))))
cam_6310 <- st_multilinestring(list(aux_1,aux_2,aux_3))


modena_roads <- st_sfc(list(cam_20937,cam_20939,cam_20936,cam_634,cam_6310), crs="EPSG:4326")

d = st_sf(data.frame(LinkID_group=c("20937", "20939", "20936","634","6310"), geom=modena_roads))

#--TODO: eliminar antes de crear, el append no funciona
st_write(d, paste0(getwd(), "/../Data/test_roads.csv"), driver = "CSV", layer_options = "GEOMETRY=AS_WKT",append=FALSE)