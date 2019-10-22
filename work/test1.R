require(hydroCR)
require(rcm2019)

setwd('/home/mha/Dropbox/melichar/data/GIS/')

s = readOGR('PLO40hranice_S-JTSK_Krovak_East_North.shp')
ws = krov2wgs(s)

setwd(file.path(Sys.getenv('R_DATA_PATH'), 'RCM2019'))
#setwd('/media/mha/KINGSTON/RCM2019/')

p = brick('pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_SMHI-RCA4_v1_day_corr_19710101_21001231.nc')
plot(p[[100]])

plot(rotatePoly(ws), add = TRUE)
