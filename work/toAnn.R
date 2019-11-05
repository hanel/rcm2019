setwd('/media/mha/KINGSTON/DATA/RCM2019/')

d= dir()

for (i in d){
  
  cat(i, '\n')
#  setwd('/media/mha/KINGSTON/DATA/RCM2019/')
  system(paste('cdo yearmean', i, file.path('/media/mha/KINGSTON/DATA/RCM2019_annual', i)))
  
}