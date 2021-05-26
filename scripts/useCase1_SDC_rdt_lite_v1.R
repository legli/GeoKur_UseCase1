
## Extract and analyze data for GeoKur use case 1 (test)
# Author: Lukas Egli & Stefano Della Chiesa
# Date: 25/05/2021

#---- Load required packages
if (!require("ckanr")) install.packages("ckanr");library ("ckanr")
if (!require("raster")) install.packages("raster");library ("raster")
if (!require("rgdal")) install.packages("rgdal");library ("rgdal")

# ----
## SET Working Directory
setwd("C:/Users/chiesa/OneDrive - GeoSci/01_Personal/00_R/14_GeoKur_UseCase1/scripts")


# ----
## CONFIGURE CKAN CONNECITON
#(BEFORE making public the repository REMEMBER to remove from history the CAN API KEY) 
ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "d5ac40d9-4f7b-472c-8e99-d7323ff41467")

# ----
## LOAD LOCALLY REMOTE RESOURCE FROM CKAN
# Retrieve Permanent crops for human consumption and pollination
pollination <- raster(resource_show(id = "09be0d5d-e67a-43ce-b80b-7e27861bbd13")$url)
yieldRapeseed <- raster(resource_show(id = "945acf8d-925f-44c5-8f45-4b6354f1734d")$url)


# ----
## DATA PREPROCESSING

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


write.csv(outputTableFinal,"myOutputTable.csv")

#---- 
## DATA ANALYSIS
# @prov [modelRapeseed] = lm(yieldRapeseed,pollinationRes) {core concept}
# using original inputs instead of the output table is maybe more informative regarding provenance?
modelRapeseed <- lm(yieldRapeseed~pollination,data=outputTableFinal) ## this would be the MODEL OUTPUT!


sink("LinearModelOutput.txt")
print(summary(modelRapeseed))
sink()  # returns output to the console

# ----
## CREATE PACKAGE & PUSH IT TO CKAN
res <- package_create(name = "test-file-yields-and-pollination-europe",
                      owner_org = "tud",
                      extras = c(was_generated_by = "test", contact_name = "arne", theme = "test"))


#----
## CREATE RESOURCE (outputTableFinal) & PUSH IT TO CKAN --> UPDATE PACKAGE
#file <- system.file("examples", outputTableFinal, package = "ckanr")
(xx <- resource_create(package_id = res$id,
                       description = "mymyOutputTableFinal  resource",
                       name = "myOutputTableFinal",
                       upload = "./myOutputTable.csv",
                       rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
))

## CREATE RESOURCE (LinearModelOutput) & PUSH IT TO CKAN --> UPDATE PACKAGE
(xx <- resource_create(package_id = res$id,
                       description = "my LinearModelOutput resource",
                       name = "LinearModelOutput",
                       upload = "./LinearModelOutput.txt",
                       rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
))


#ds_create(resource_id = xx$id, records = iris, force = TRUE)
#resource_show(xx$id)


#rm(list=ls())
