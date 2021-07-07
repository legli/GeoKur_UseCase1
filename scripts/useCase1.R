
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
ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE2MjM0MTk5MTAsImp0aSI6Imprc0FrVnlRTHMzT1hjQS1PZXZjNGRVck1sc3g5SmptaGNKUmEyMjNNWWRhVkc0WHBKX3BuU3daSEZtS2tqVm03a2JUb0JEd19zT1VIN2Y4In0.HvWSVGr6RYmKXkyLCNQtMVmM_GB-dGLsJ9xtzLiQwW4")


# browse ckan available datasets, package list gives the human-readable identifieres of every public dataset. The API refers to those human-readable identifieres as
# "name". In the CKAN Webpage we call them "Identifier".
ckan_available_datasets <- package_list()

# access the metadata of a given dataset by its "name"
# package_show returns an R List object
pollination_metadata <- package_show("demand-and-supply-of-pollination-in-the-european-union-test") 
yieldRapeseed_metadata <- package_show("rapeseed-yield")

# ----
## LOAD LOCALLY REMOTE RESOURCE FROM CKAN
# Retrieve Permanent crops for human consumption and pollination
# get resource download url (target resource is nb. one in resource list)
target_index <- 1
download_url_pollination <- pollination_metadata$resource[[target_index]]$url
download_url_yieldRapessed <- yieldRapeseed_metadata$resource[[target_index]]$url
pollination <- raster(download_url_pollination)
yieldRapeseed <- raster(download_url_yieldRapessed)


########################## DATA PREPROCESSING
# # project raster
pollinationProj <- projectRaster(pollination, crs="+proj=longlat +datum=WGS84 +no_defs ") # change crs
# ---- CREATE INTERMEDIATE DATASET 1 ----
intermediate_dataset_pollination_proj <- package_create(
  extras = c(
    name = "intermediate_dataset_pollination_proj",
    conforms_to = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = pollination_metadata$name
  )
)
# at this point we could also upload the reprojected resource

# # resample to 5 arcmin
pollinationRes <- resample(pollinationProj,yieldRapeseed) 
# ---- CREATE INTERMEDIATE DATASET 2 ----
intermediate_dataset_pollination_res <- package_create(
  extras = c(
    name = "intermediate_dataset_pollination_res",
    conforms_to = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = intermediate_dataset_pollination_proj$name
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
    name = "output_dataset",
    conforms_to = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = paste(yieldRapeseed_metadata$name, intermediate_dataset_pollination_res$name, sep=",") 
  )
)


# upload dataset
resource_create(package_id = output_dataset$id,
                      description = "myOutputTableFinal",
                      name = "myOutputTableFinal",
                      upload = "./myOutputTable.csv",
                      rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
)
                      
########################## Data analysis
modelRapeseed <- lm(yieldRapeseed~pollination,data=outputTableFinal) ## this would be the MODEL OUTPUT!
save(modelRapeseed,file="modelRapeseed.RData")

# ---- CREATE OUTPUT DATASET ----
analysis_dataset <- package_create(
  extras = c(
    name = "model_rapeseed",
    conforms_to = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    owner_org = "ufz",
    contact_name = "lukas",
    theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = output_dataset$name
  )
)
# upload dataset
resource_create(package_id = analysis_dataset$id,
                description = "myOutputTableFinal",
                name = "myOutputTableFinal",
                upload = "./modelRapeseed.Rdata",
                rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
)

# package_delete(id = intermediate_dataset_pollination_proj$name)
# package_delete(id = intermediate_dataset_pollination_res$name)
# package_delete(id = output_dataset$name)
# package_delete(id = analysis_dataset$name)


# 
# sink("LinearModelOutput.txt")
# print(summary(modelRapeseed))
# sink()  # returns output to the console
# 
# 
# ## CREATE PACKAGE & PUSH IT TO CKAN
# res <- package_create(name = "test-file-yields-and-pollination-europe",
#                       owner_org = "tud",
#                       extras = c(was_generated_by = "test", contact_name = "arne", theme = "test"))
# 
# 
# #----
# ## CREATE RESOURCE (outputTableFinal) & PUSH IT TO CKAN --> UPDATE PACKAGE
# #file <- system.file("examples", outputTableFinal, package = "ckanr")
# (xx <- resource_create(package_id = res$id,
#                        description = "myOutputTableFinal  resource",
#                        name = "myOutputTableFinal",
#                        upload = "./myOutputTable.csv",
#                        rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
# ))
# 
# ## CREATE RESOURCE (LinearModelOutput) & PUSH IT TO CKAN --> UPDATE PACKAGE
# (xx <- resource_create(package_id = res$id,
#                        description = "my LinearModelOutput resource",
#                        name = "LinearModelOutput",
#                        upload = "./LinearModelOutput.txt",
#                        rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
# ))
# 
# 
# #ds_create(resource_id = xx$id, records = iris, force = TRUE)
# resource_show(xx$id)


#rm(list=ls())
