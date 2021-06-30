
## Extract and analyze data for GeoKur use case 1 (test)
# Author: Lukas Egli
# Date: 19/05/2021

#---- Load required packages
if (!require("ckanr")) install.packages("ckanr");library ("ckanr")
if (!require("raster")) install.packages("raster");library ("raster")
if (!require("rgdal")) install.packages("rgdal");library ("rgdal")


#---- 
## PROVENANCE (in progress)
#Testing Packages for provenance tracking --> TBD


# ----
## CONFIGURE CKAN CONNECITON
#(BEFORE making public the repository REMEMBER to remove from history the CAN API KEY) 
ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "42bec965-954b-4898-bc76-a18274bbc982")

# ----
## LOAD LOCALLY REMOTE RESOURCE FROM CKAN
# Retrieve Permanent crops for human consumption and pollination
pollination <- raster(resource_show(id = "09be0d5d-e67a-43ce-b80b-7e27861bbd13")$url)
yieldRapeseed <- raster(resource_show(id = "945acf8d-925f-44c5-8f45-4b6354f1734d")$url)


# ---- CREATE INPUT 1 ----
input_dataset_pollination <- package_create(
  extras = c(
    name = "input-input_dataset_pollination",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au"
  )
)

# ---- CREATE INPUT 2 ----
input_dataset_yieldRapeseed <- package_create(
  extras = c(
    name = "input_dataset_yieldRapeseed",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au"
  )
)


########################## DATA PREPROCESSING

# project raster
pollinationProj <- projectRaster(pollination, crs="+proj=longlat +datum=WGS84 +no_defs ") # change crs
# ---- CREATE INTERMEDIATE DATASET 1 ----
intermediate_dataset_pollinationProj <- package_create(
  extras = c(
    name = "intermediate_dataset_pollinationProj",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = input_dataset_pollination$name,
    was_generated_by = '{"uri": "XYZ", "label": "Project"}'
  )
)

# resample to 5 arcmin
pollinationRes <- resample(pollinationProj,yieldRapeseed) 
# ---- CREATE INTERMEDIATE DATASET 2 ----
intermediate_dataset_pollinationRes <- package_create(
  extras = c(
    name = "intermediate_dataset_pollinationRes",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = intermediate_dataset_pollinationProj$name,
    was_generated_by = '{"uri": "XYZ", "label": "Resample"}'
  )
)
plot(pollinationRes,xlim=c(-20,50),ylim=c(20,70))
plot(yieldRapeseed,xlim=c(-20,50),ylim=c(20,70))


# combine pollination and yield data  to table
outputTable <- cbind(as.data.frame(yieldRapeseed),as.data.frame(pollinationRes)) 
names(outputTable) <- c("yieldRapeseed","pollination")
# remove 0 yields and NAs
outputTableFinal <- outputTable[which(outputTable$yieldRapeseed>0&!is.na(outputTable$pollination)),] 
head(outputTableFinal) ## this would be the DATA OUTPUT!

write.csv(outputTableFinal,"myOutputTable.csv")


# ---- CREATE OUTPUT DATASET ----
output_dataset <- package_create(
  extras = c(
    name = "output-dataset",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = paste(input_dataset_yieldRapeseed$name, intermediate_dataset_pollinationRes$name, sep=","), 
    was_generated_by = '{"uri": "XYZ", "label": "Cbind"}'
  )
)









########################## Data analysis
# @prov [modelRapeseed] = lm(yieldRapeseed,pollinationRes) {core concept}
# using original inputs instead of the output table is maybe more informative regarding provenance?
modelRapeseed <- lm(yieldRapeseed~pollination,data=outputTableFinal) ## this would be the MODEL OUTPUT!


sink("LinearModelOutput.txt")
print(summary(modelRapeseed))
sink()  # returns output to the console


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
resource_show(xx$id)


#rm(list=ls())
