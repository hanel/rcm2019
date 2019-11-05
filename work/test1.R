require(hydroCR)
require(rcm2019)

setwd('/home/mha/Dropbox/melichar/data/GIS/')

s = readOGR('PLO40hranice_S-JTSK_Krovak_East_North.shp')
ws = krov2wgs(s)

setwd(file.path(Sys.getenv('R_DATA_PATH'), 'RCM2019', 'annual'))
#setwd('/media/mha/KINGSTON/RCM2019/')

p = brick('pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_SMHI-RCA4_v1_day_corr_19710101_21001231.nc')
plot(p[[1]])

plot(rotatePoly(ws), add = TRUE)


fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
}

fillp = brick(p, filename = tempfile())

i = 1
for (i in 1:nlayers(fillp)){
  fillp[[i]] <- focal(p[[i]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )
}


pfill <- focal(p[[1]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )


setdp('chars')

preci = raster('srazky_8110.nc')
writeRaster(preci, 'srazky_8110.nc')


rpreci = raster('rot_srazky.nc')
r = raster('BF_krasny.tif')

pr = projectRaster(from = r, crs = projection(p))

writeRaster(pr, 'BF_krasny.tif')

rr = raster('BF_krasny.tif')

projectRaster(from = preci, crs = )



xy = coordinates(preci)

rxy = SpatialPointsDataFrame(rotated_grid_transform(xy),  data = data.frame(values(preci)))


y = raster(nrows = nrow(preci), ncols = ncol(preci), ext = extent(p))
xy = rotated_grid_transform(coordinates(y), option = '2lon')



rrr = rasterize(rxy, y, field = 1)


rrr = resample(rxy, y)
