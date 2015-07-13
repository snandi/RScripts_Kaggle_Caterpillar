## Choose USA (IA) as the CRAN mirror
chooseCRANmirror(graphics=F, ind=83)

Packages <- c(
  'boot',
  'car',
  'ggplot2',
  'lattice',
  'plyr',
  'reshape',
  'reshape2'
)

## For loop for requiring packages and installing them if something doesnt exist
for(Package in Packages){
  if(require(package=Package, character.only=T) == F){
    print(paste('Installing', Package))
    try(install.packages(Package, dependencies = TRUE))
  } else{
    print(paste(Package, 'already exists'))
    require(package=Package, character.only=T)
  }
}

## For parallel processing, when passing the list of packages to load
## in all the cores. Could be different from Packages
MyAutoLoads <- Packages