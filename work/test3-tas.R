require(hydroCR)
require(rcm2019)


fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm = TRUE), 4) )
  } else {
    return( x[i] )
  }
}

preciwei = function(x){

  w = x/sum(x)
  return(w/sum(w))
}

################

setwd('/home/mha/Dropbox/melichar/data/GIS/')
s = readOGR('PLO40hranice_S-JTSK_Krovak_East_North.shp')
ws = krov2wgs(s)
rws = rotatePoly(ws)

dem = raster('srtm_krov500r.tif')
projection(dem) = s@proj4string
asp = terrain(dem, opt = 'aspect')
slo = terrain(dem, opt = 'slope')

xy = coordinates(dem)
xy = krov2wgs(SpatialPoints(xy))

rxy = rotatePoints(xy)
rxy = SpatialPointsDataFrame(rxy, data = data.frame(ele = values(dem)))

setdp('chars')

temp = raster('teplota_8110.tif')
ktemp = projectRaster(temp, crs = s@proj4string)

tmp_obs = cellStats(mask(ktemp, s), mean)

preci = crop(temp, buffer(ws, 0.2))
pxy = coordinates(preci)
rpxy = rotatePoints(SpatialPoints(pxy))
rpxy = SpatialPointsDataFrame(rpxy, data = data.frame(PR = values(preci)))

finepreci = resample(ktemp, dem, method = 'ngb')

#ktemp = crop(ktemp, buffer(s, 10000))

setwd(file.path(Sys.getenv('R_DATA_PATH'), 'RCM2019', 'annual'))
#setwd('/media/mha/KINGSTON/RCM2019/')


#f = 'pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_SMHI-RCA4_v1_day_corr_19710101_21001231.nc'

d = dir(pattern = 'tas_')

f = d[1]

for (f in d){

  setdp(file.path('MELICHAR', 'TAS'))
  if (file.exists(f)) next

  setwd(file.path(Sys.getenv('R_DATA_PATH'), 'RCM2019', 'annual'))

  cat('\n', f, '\n')
  
  p = brick(f, varname = "tas")


  cp = crop(p, buffer(rws, width = .2)) - 273.15
  
  rdem = rasterize(rxy, cp, fun = mean)[[2]]
  ae = cellStats(rdem, mean)
  lscor = ((rdem-ae)/100)*0.65
  
  cp = cp + lscor # remove the influence of elevation
  
  fillp = brick(cp)
  values(fillp) = NA_real_

  i = 1
  for (i in 1:nlayers(fillp)){
    fillp[[i]] <- focal(cp[[i]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )
  }


  fillp = disaggregate(fillp, fact = 10, method = 'bilinear')

  e = extract(fillp, rxy)
  out = brick(dem, nl = nlayers(p))
  values(out) = e
  
  aele = cellStats(mask(dem, s), mean)
  cdem = ((aele-dem)/100)*0.65

  RES = out + cdem
  names(RES) = year(unique(p@z$Date))
  RES@z = list(unique(year(p@z$Date)))
  
  mea = cellStats(mask(mean(RES[[ which(RES@z[[1]] %in% c(1981:2010)) ]]), s), mean)
  
  RES = RES + tmp_obs - mea
  
  RES = mask(crop(RES, s), s)
  
  RES@z = list(unique(year(p@z$Date)))
  
  setdp(file.path('MELICHAR', 'TAS'))
  writeRaster(RES, filename = f, overwrite = TRUE, varname = 'tas', varunit = 'degree C', longname = 'temperature', xname = 'x', yname = 'y', zname = 'time', zunit = 'year')

}

####

