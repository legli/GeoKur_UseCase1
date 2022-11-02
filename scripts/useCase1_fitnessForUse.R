
## Description: Extract data, assess fitness for use and analyze data in a stylized way
## for GeoKur use case 1 
# Author: Lukas Egli
# Date: 02/11/2022

############# Load required packages
if (!require("ckanr")) install.packages("ckanr");library ("ckanr")
if (!require("httr")) install.packages("httr");library ("httr")
if (!require("jsonlite")) install.packages("jsonlite");library ("jsonlite")

if (!require("raster")) install.packages("raster");library ("raster")
if (!require("rgdal")) install.packages("rgdal");library ("rgdal")
if (!require("sf")) install.packages("sf");library ("sf")
if (!require("tidyr")) install.packages("tidyr");library ("tidyr")


############# Function to check provenance
get_origin_datasets <- function(dataset_uri, endpoint = "https://geokur-dmp2.geo.tu-dresden.de/fuseki/ckan_mirror/sparql") {
  query <- sprintf("SELECT ?dataset WHERE {<%s> <http://www.w3.org/ns/prov#wasDerivedFrom> ?dataset .}", dataset_uri)
  request <- sprintf("%s?query=%s", endpoint, URLencode(query, reserved = TRUE))
  response <- httr::GET(request)
  res_df <- jsonlite::fromJSON(httr::content(response, "text"))$result$bindings$dataset$value
  if (length(res_df)) {
    return(res_df)
  } else {
    return(FALSE)
  }
}

############# Set some useful variables for later
dataset_base_url <- "https://geokur-dmp.geo.tu-dresden.de/dataset/"
process_base_url <- "https://geokur-dmp.geo.tu-dresden.de/process/"
workflow_base_url <- "https://geokur-dmp.geo.tu-dresden.de/workflow/"


############# CONFIGURE CKAN CONNECITON
#(BEFORE making public the repository REMEMBER to remove from history the CAN API KEY) 
ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "XYZ")

# browse ckan available datasets, package list gives the human-readable identifieres of every public dataset. The API refers to those human-readable identifieres as
# "name". In the CKAN Webpage we call them "Identifier".
ckan_available_datasets <- package_list()

########################## STEP 1: SEARCH AND DOWNLOAD DATA
############ STEP 2.2: 
yieldDatasets <- package_search(fq="tags:(yield OR Yield)")$results
pollinationDatasets <- package_search(fq="tags:(Pollinat* OR pollinat*)")$results
irrigationDatasets <- package_search(fq="tags:(Irrig* OR irrig*)")$results

mapSpam_metadata <- package_show(yieldDatasets[[2]])
monfreda_metadata <- package_show(yieldDatasets[[1]])
pollination_metadata <- package_show(pollinationDatasets[[1]]) 
pollination_points_metadata <- package_show(pollinationDatasets[[2]]) 
irrigation_metadata <- package_show(irrigationDatasets[[1]]) 

# download data
download_url_rapeseed <- monfreda_metadata$resource[[1]]$url
download_url_rapeseedQuality <- monfreda_metadata$resource[[2]]$url
download_url_pollination <- pollination_metadata$resource[[1]]$url
download_url_pollination_points <- pollination_points_metadata$resource[[1]]$url
download_url_irrigation <- irrigation_metadata$resource[[1]]$url

yieldRapeseed <- raster(download_url_rapeseed)
yieldRapeseedQuality <- raster(download_url_rapeseedQuality)
irrigationRapeseed <- raster(download_url_irrigation)
pollination <- raster(download_url_pollination)

## some initial data processing
# define crs
crs(irrigationRapeseed) <- "+proj=longlat +datum=WGS84 +no_defs "
# project raster
pollinationProj <- projectRaster(pollination, crs="+proj=longlat +datum=WGS84 +no_defs ") # change crs
# resample to 5 arcmin
pollinationRes <- resample(pollinationProj,yieldRapeseed) 

plot(pollinationRes,xlim=c(-20,50),ylim=c(20,70))
plot(yieldRapeseed,xlim=c(-20,50),ylim=c(20,70))
plot(irrigationRapeseed,xlim=c(-20,50),ylim=c(20,70))


########################## STEP 2: ASSESS FITNESS FOR USE FOR YIELD DATA
############ STEP 2.1: ASSESS PROVENANCE
package_show(ckan_available_datasets[[22]])$id
inputDatasets <- get_origin_datasets(paste0(dataset_base_url,"b0e5c26c-7762-4f99-8234-b793ce13d19c"))
sapply(1:length(inputDatasets),function(i){
   package_show(tail(strsplit(inputDatasets[i],"/")[[1]],1))
  })
## -> Decision I: reject map SPAM  (irrigation as input -> circular reasoning)

############ STEP 2.2: ASSESS SPATIALLY EXPLICIT DATA QUALITY OF MONFREDA
plot(yieldRapeseedQuality,xlim=c(-20,50),ylim=c(20,70))
tableQuality <- cbind(as.data.frame(yieldRapeseedQuality),as.data.frame(pollinationRes))
tableQuality <- tableQuality[which(!is.na(tableQuality$X3b_visitprob)&tableQuality$rapeseed_dataquality_yield>0),]
head(tableQuality)
mean(tableQuality$rapeseed_dataquality_yield)
## -> Decision II: accept monfreda due to high quality in Europe


########################## STEP 3: DATA PROCESSING AND ANALYSIS
############ STEP 3.1: combine pollination and yield data  to table
outputTable <- cbind(as.data.frame(yieldRapeseed),as.data.frame(pollinationRes),as.data.frame(irrigationRapeseed)) 
names(outputTable) <- c("yieldRapeseed","pollination","irrigationRapeseed")
# remove 0 yields and NAs
outputTableFinal <- outputTable[which(outputTable$yieldRapeseed>0&!is.na(outputTable$pollination)),] 
head(outputTableFinal) ## this would be the DATA OUTPUT!
write.csv(outputTableFinal,"myOutputTable.csv")

############ STEP 3.2: model rapeseed yield (stylized model)
modelRapeseed <- lm(yieldRapeseed~pollination+irrigationRapeseed,data=outputTableFinal) ## this would be the MODEL OUTPUT!
save(modelRapeseed,file="modelRapeseed.RData")


########################## STEP 4: ADD DATA TO CKAN
############ STEP 4.1: UPLOAD OUTPUT TABLE (METADATA, RESOURCE, PROCESS)
# metadata
output_dataset <- package_create(
  extras = c(
    name = "output_dataset",
    title = "Output dataset",
    description = "",
    owner_org = "ufz",
    contact_name = "lukas egli",
    was_derived_from = paste(
      paste0(dataset_base_url,monfreda_metadata$id), 
      paste0(dataset_base_url,pollination_metadata$id),
      paste0(dataset_base_url,irrigation_metadata$id), sep=",")
  )
)

# output_dataset <- package_show("output_dataset")
# package_delete(id = output_dataset$name)

# resource
resource_create(package_id = output_dataset$id,
                      # description = "myOutputTableFinal",
                      name = "myOutputTableFinal",
                      upload = "./myOutputTable.csv",
                      rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
)

# process
cbind_metadata <- package_create(
  extras = c(
    type = "process",
    name = "combine_rapeseed_and_pollination",
    title = "Combine Rapeseed And Pollination",
    notes = "Bind rapeseed yield and pollination rasters to common table and remove rapeseed values equal to zero and pollination values that are not defined.",
    owner_org = "ufz",
    contact_name = "lukas egli",
    used = paste(
      paste0(dataset_base_url, monfreda_metadata$id),
      paste0(dataset_base_url, pollination_metadata$id),
      paste0(dataset_base_url, irrigation_metadata$id),
      sep=","),
    generated = paste0(dataset_base_url, output_dataset$id)
    # category = "geokur:Selection"
  )
)
# cbind_metadata <- package_show("combine_rapeseed_and_pollination")
# package_delete(id = cbind_metadata$name)
                      

############ STEP 4.2: MODEL (METADATA, RESOURCE, PROCESS)
# metadata
model_rapeseed_output_metadata <- package_create(
  extras = c(
    name = "model_rapeseed_output",
    title = "Model rapeseed output",
    # conforms_to = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    owner_org = "ufz",
    contact_name = "lukas egli",
    # theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = paste0(dataset_base_url, output_dataset$id)
  )
)
# model_rapeseed_output_metadata <- package_show("model_rapeseed_output")
# package_delete(id = model_rapeseed_output_metadata$name)

# resource
resource_create(package_id = model_rapeseed_output_metadata$id,
                description = "myModelOutput",
                name = "myOutputTableFinal",
                upload = "./modelRapeseed.Rdata",
                rcurl = "https://geokur-dmp.geo.tu-dresden.de/dataset"
)

# process
model_rapeseed_metadata <- package_create(
  extras = c(
    type = "process",
    name = "model_rapeseed",
    title = "Model rapeseed",
    notes = "linear regression model relating rapeseed yield to pollination",
    owner_org = "ufz",
    contact_name = "lukas egli",
    used = paste0(dataset_base_url, output_dataset$id),
    generated = paste0(dataset_base_url, model_rapeseed_output_metadata$id)
    # category = "geokur:Selection"
  )
)
# model_rapeseed_metadata <- package_show("model_rapeseed")
# package_delete(id = model_rapeseed_metadata$name)

rm(list=ls())
