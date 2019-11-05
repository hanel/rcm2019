require(hydroCR)
require(rcm2019)
require(ncdf.tools)

setwd('/home/mha/Dropbox/melichar/data/GIS/')


dem = raster('srtm_krov500r.tif')

xy = coordinates(dem)
xy = krov2wgs(SpatialPoints(xy))

rxy = rotatePoints(xy)
obr = readOGR('PLO40hranice_S-JTSK_Krovak_East_North.shp')


setwd(file.path(Sys.getenv('R_DATA_PATH'), 'RCM2019_annual'))
#setwd('/media/mha/KINGSTON/RCM2019/')

p = brick('pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_SMHI-RCA4_v1_day_corr_19710101_21001231.nc')

d = distance(p[[1]])


plot(p[[100]])
points(rxy)

e = extract(p[[1]], rxy, cellnum = TRUE)

dta = brick(nrows = nrow(dem), ncols = ncol(dem), xmn = xmin(dem), xmx = xmax(dem), ymn = ymin(dem), ymx = ymax(dem), nl = nlayers(p))

values(dta) = e

plot(rotatePoly(ws), add = TRUE)

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
