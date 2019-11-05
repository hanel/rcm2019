
setwd('/media/mha/KINGSTON/DATA/RCM2019/')

d= dir()

for (i in d){

  cat(i, '\n')
#  setwd('/media/mha/KINGSTON/DATA/RCM2019/')
  system(paste('cdo yearmean', i, file.path('/media/mha/KINGSTON/DATA/RCM2019_annual', i)))

}


setwd('/home/DATA/RCM2019/daily')

d= dir()

for (i in d){

  cat(i, '\n')
  #  setwd('/media/mha/KINGSTON/DATA/RCM2019/')
  system(paste('cdo monmean', i, file.path('/home/DATA/RCM2019/monthly', i)))

}
