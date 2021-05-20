
## Extract and analyze data for GeoKur use case 1 (test)
# Author: Lukas Egli
# Date: 19/05/2021

if (!require("ckanr")) install.packages("ckanr");library ("ckanr")
if (!require("raster")) install.packages("raster");library ("raster")


########################## Data download
## Connect to GeoKur CKAN and retrieve Permanent crops for human consumption
ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/")
pollination <- raster(resource_show(id = "09be0d5d-e67a-43ce-b80b-7e27861bbd13")$url)
yieldRapeseed <- raster(resource_show(id = "945acf8d-925f-44c5-8f45-4b6354f1734d")$url)



########################## Data preparation
# @prov [pollinationProj] = projectRaster(pollination) {basal change}
pollinationProj <- projectRaster(pollination, crs="+proj=longlat +datum=WGS84 +no_defs ") # change crs
# @prov [pollinationRes] = resample(pollination,yieldRapeseed) {value change}
pollinationRes <- resample(pollinationProj,yieldRapeseed) # resample to 5 arcmin
plot(pollinationRes,xlim=c(-20,50),ylim=c(20,70))
plot(yieldRapeseed,xlim=c(-20,50),ylim=c(20,70))

# @prov [outputTable] = cbind(yieldRapeseed,pollinationRes) {basal change} -> Intermediate step: maybe remove
outputTable <- cbind(as.data.frame(yieldRapeseed),as.data.frame(pollinationRes)) # rearrange to table
names(outputTable) <- c("yieldRapeseed","pollination")

# @prov [outputTableFinal] = cbind(yieldRapeseed,pollinationRes) {basal change} -> Intermediate step: maybe remove
outputTableFinal <- outputTable[which(outputTable$yieldRapeseed>0&!is.na(outputTable$pollination)),] # remove 0 yields and NAs
head(outputTableFinal) ## this would be the DATA OUTPUT!

# store data -> not working yet!!!
res <- package_create("Yields and pollination Europe", author="Lukas Egli")
# then create a resource
file <- system.file("examples", outputTableFinal, package = "ckanr")
(xx <- resource_create(package_id = res$id,
                       description = "my resource",
                       name = "bears",
                       upload = file,
                       rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
))
ds_create(resource_id = xx$id, records = iris, force = TRUE)
resource_show(xx$id)

########################## Data analysis
# @prov [modelRapeseed] = lm(yieldRapeseed,pollinationRes) {core concept}
# using original inputs instead of the output table is maybe more informative regarding provenance?
modelRapeseed <- lm(yieldRapeseed~pollination,data=outputTableFinal) ## this would be the MODEL OUTPUT!
summary(modelRapeseed)

rm(list=ls())
