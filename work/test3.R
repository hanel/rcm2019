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

xy = coordinates(dem)
xy = krov2wgs(SpatialPoints(xy))

rxy = rotatePoints(xy)

setdp('chars')

preci = raster('srazky_8110.nc')
kpreci = projectRaster(preci, crs = s@proj4string)

#temp = raster('teplota_8110.tif')
#ktemp = projectRaster(temp, crs = s@proj4string)

preci = crop(preci, buffer(ws, 0.2))
pxy = coordinates(preci)
rpxy = rotatePoints(SpatialPoints(pxy))
rpxy = SpatialPointsDataFrame(rpxy, data = data.frame(PR = values(preci)))

finepreci = resample(kpreci, dem, method = 'ngb')

#ktemp = crop(ktemp, buffer(s, 10000))

setwd(file.path(Sys.getenv('R_DATA_PATH'), 'RCM2019', 'annual'))
#setwd('/media/mha/KINGSTON/RCM2019/')

p = brick('pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_SMHI-RCA4_v1_day_corr_19710101_21001231.nc')


  cp = crop(p, buffer(rws, width = .2)) / 10 * 365.25

  fillp = brick(cp)
  values(fillp) = NA_real_

  i = 1
  for (i in 1:nlayers(fillp)){
    fillp[[i]] <- focal(cp[[i]], w = matrix(1,3,3), fun = fill.na, pad = TRUE, na.rm = FALSE )
  }

  e = extract(fillp, rxy)
  out = brick(dem, nl = nlayers(p))
  values(out) = e

  dta = data.table(coordinates(out[[1]]), fiPR = values(finepreci), rPR = values(out[[1]]), ele = values(dem))

  dta[, nx := scale(x)]
  dta[, ny := scale(y)]
  dta[, nrPR := scale(rPR)]
  dta[, nele := scale(ele)]

  m = lm(fiPR ~ x + y + rPR + ele, data = dta)
  dta[(!is.na(fiPR) & !is.na(nele) & !is.na(nrPR)), PR := predict(m)]

  res = raster(out)
  values(res) = dta$PR



####

pp = cp[[1]]
dpp = disaggregate(pp, fact = 10, method = 'bilinear')
