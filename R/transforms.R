#' Transforms coordinates to rotated grid
#'
#' @param grid_in matrix of lon-lat coordinates
#' @param option either `2rot` - transforms to rotated grid, other values transform from rotated grid to lon-lat
#' @param SP_coor a vector of `grid_north_pole_latitude` and `grid_north_pole_longitude` from the netcdf file.
#'
#' @return data.table with coordinates
#' @export rotated_grid_transform
#'
#' @examples
rotated_grid_transform = function(grid_in, option = '2rot', SP_coor = c(39.25, -162)){

  lon = grid_in[, 1]
  lat = grid_in[, 2]

  lon = (lon*pi)/180; # Convert degrees to radians
  lat = (lat*pi)/180;

  SP_lon = SP_coor[2];
  SP_lat = SP_coor[1];

  theta = 90+SP_lat; # Rotation around y-axis
  phi = SP_lon; # Rotation around z-axis

  theta = (theta*pi)/180;
  phi = (phi*pi)/180; # Convert degrees to radians

  x = cos(lon)*cos(lat); # Convert from spherical to cartesian coordinates
  y = sin(lon)*cos(lat);
  z = sin(lat);

  if (option == '2rot'){ # Regular -> Rotated

    x_new = cos(theta)*cos(phi)*x + cos(theta)*sin(phi)*y + sin(theta)*z;
    y_new = -sin(phi)*x + cos(phi)*y;
    z_new = -sin(theta)*cos(phi)*x - sin(theta)*sin(phi)*y + cos(theta)*z;

  }  else {  # Rotated -> Regular

    phi = -phi;
    theta = -theta;

    x_new = cos(theta)*cos(phi)*x + sin(phi)*y + sin(theta)*cos(phi)*z;
    y_new = -cos(theta)*sin(phi)*x + cos(phi)*y - sin(theta)*sin(phi)*z;
    z_new = -sin(theta)*x + cos(theta)*z;

  }

  lon_new = atan2(y_new,x_new); # Convert cartesian back to spherical coordinates
  lat_new = asin(z_new);

  lon_new = (lon_new*180)/pi; # Convert radians back to degrees
  lat_new = (lat_new*180)/pi;

  if (option == '2rot') return(data.table(X = -lon_new, Y = -lat_new)) else return(data.table(lon_new = lon_new, lat_new = lat_new))

}

#rotated_grid_transform(rotated_grid_transform(t(c(14.12, 50.5)) ))


#' Transforms shapefile to rotated coordinates
#'
#' The functions uses `rcm2019::rotated_grid_transform` to rotate coordinates of the polygons from the shape file
#'
#' @param shp SpatialPolygons or SpatialPolygonsDataFrame
#'
#' @return SpatialPolygons at rotated coordinates
#' @export rotatePoly
#'
#' @examples
rotatePoly = function(shp, ...){

  E = NULL
  gr = shp[1, ]@polygons[[1]]@Polygons[[1]]@coords
  E = SpatialPolygons(list(Polygons(list(Polygon(rotated_grid_transform(gr, option = '2rot', ...) )), shp[1, ]@polygons[[1]]@ID)))

  if (length(shp)>1){

    for (i in 2:length(shp)){

      gr = shp[i, ]@polygons[[1]]@Polygons[[1]]@coords
      e = SpatialPolygons(list(Polygons(list(Polygon(rotated_grid_transform(gr, option = '2rot', ...) )), shp[i, ]@polygons[[1]]@ID)))

      E = rbind(E, e)
    }
  }
  return(E)
}


#' Rotate spatial points
#'
#' The functions uses `rcm2019::rotated_grid_transform` to rotate coordinates of the points from the shape file
#'
#' @param shp SpatialPoints or SpatialPointsDataFrame
#' @param ...
#'
#' @return SpatialPoints at rotated coordinates
#' @export rotatePoints
#'
#' @examples
rotatePoints = function(shp, ...){

  pom = rotated_grid_transform(shp@coords, option = '2rot')
  return(SpatialPoints(pom))

}
