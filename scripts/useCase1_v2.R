
## Extract and analyze data for GeoKur use case 1 (test)
# Author: Lukas Egli
# Date: 08/12/2021

#---- Load required packages
if (!require("ckanr")) install.packages("ckanr");library ("ckanr")
if (!require("httr")) install.packages("httr");library ("httr")
if (!require("jsonlite")) install.packages("jsonlite");library ("jsonlite")

if (!require("raster")) install.packages("raster");library ("raster")
if (!require("rgdal")) install.packages("rgdal");library ("rgdal")
if (!require("sf")) install.packages("sf");library ("sf")
if (!require("tidyr")) install.packages("tidyr");library ("tidyr")


#---- Function to check provenance
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

#---- Load required packages
# Set some useful variables for later
dataset_base_url <- "https://geokur-dmp.geo.tu-dresden.de/dataset/"
process_base_url <- "https://geokur-dmp.geo.tu-dresden.de/process/"
workflow_base_url <- "https://geokur-dmp.geo.tu-dresden.de/workflow/"

# ----
## CONFIGURE CKAN CONNECITON
#(BEFORE making public the repository REMEMBER to remove from history the CAN API KEY) 
ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "42bec965-954b-4898-bc76-a18274bbc982")
# ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE2MjM0MTk5MTAsImp0aSI6Imprc0FrVnlRTHMzT1hjQS1PZXZjNGRVck1sc3g5SmptaGNKUmEyMjNNWWRhVkc0WHBKX3BuU3daSEZtS2tqVm03a2JUb0JEd19zT1VIN2Y4In0.HvWSVGr6RYmKXkyLCNQtMVmM_GB-dGLsJ9xtzLiQwW4")
# ckanr_setup("https://geokur-dmp.geo.tu-dresden.de/", key = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE2MjU2NjY0MDksImp0aSI6ImNacGlneDFXcFVzUHNUS2JOLWtjRU5KbVFBVzZiSzZRaHktcXFFQU9FeEhrTWx6emZlLXlkemc2UjhJd25PZlU3Ti1uVnROcU5HM1ozaVREIn0.yZDqt8WEGFJV6iZ13_L5EWwTvId3zQgpxiU0Rrhk0zM")

# browse ckan available datasets, package list gives the human-readable identifieres of every public dataset. The API refers to those human-readable identifieres as
# "name". In the CKAN Webpage we call them "Identifier".
ckan_available_datasets <- package_list()

########################## STEP 1: SEARCH AND DOWNLOAD DATA
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

temp=tempfile()
download.file(download_url_pollination_points, temp)
unzip(temp)
pollinationPoints <- readOGR(dsn = ".", layer = "Martin_pollinator data extracted")

## some initial data processing
# define crs
crs(irrigationRapeseed) <- "+proj=longlat +datum=WGS84 +no_defs "
# project raster
pollinationProj <- projectRaster(pollination, crs="+proj=longlat +datum=WGS84 +no_defs ") # change crs
# resample to 5 arcmin
pollinationRes <- resample(pollinationProj,yieldRapeseed) 

plot(pollinationRes,xlim=c(-20,50),ylim=c(20,70))
plot(pollinationPoints,xlim=c(-20,50),ylim=c(20,70),add=T)
plot(yieldRapeseed,xlim=c(-20,50),ylim=c(20,70))
plot(irrigationRapeseed,xlim=c(-20,50),ylim=c(20,70))

########################## STEP 2: ASSESS FITNESS FOR USE FOR YIELD DATA
############ STEP 2.1: ASSESS PROVENANCE
package_show(ckan_available_datasets[[22]])$id
inputDatasets <- get_origin_datasets(paste0(dataset_base_url,"b0e5c26c-7762-4f99-8234-b793ce13d19c"))
sapply(1:length(inputDatasets),function(i){
   package_show(tail(strsplit(inputDatasets[i],"/")[[1]],1))
  })

## -> Decision I: reject map SPAM  (irrigation as input -> circularities)

############ STEP 2.2: ASSESS SPATIALLY EXPLICIT DATA QUALITY OF MONFREDA
plot(yieldRapeseedQuality,xlim=c(-20,50),ylim=c(20,70))
tableQuality <- cbind(as.data.frame(yieldRapeseedQuality),as.data.frame(pollinationRes))
tableQuality <- tableQuality[which(!is.na(tableQuality$layer)&tableQuality$rapeseed_dataquality_yield>0),]
head(tableQuality)
mean(tableQuality$rapeseed_dataquality_yield)
## -> Decision II: accept monfreda due to high quality in Europe

########################## STEP 3: ASSESS FITNESS FOR USE FOR POLLINATION DATA
############ STEP 3.1: CHECK DATA QUALITY WITH INDEPENDENT DATA
#### pollination 
## get sites
pointID <- pollinationPoints[,c("SiteID")]
pointID <- pointID[!duplicated(pointID$SiteID),]

## aggregate over years
pollinationPointsMeanAbundanceSpecies <- aggregate(Abundnc~SiteID+SpecsID+Txnmc_g,pollinationPoints,function(i){mean(i,na.rm=T)})
sum(duplicated(pollinationPointsMeanAbundanceSpecies [c("SiteID","SpecsID")]))

# total abundances
pollinationPointsSumAbundance <- aggregate(Abundnc~SiteID,pollinationPointsMeanAbundanceSpecies,function(i){sum(i,na.rm=T)})

## combine data
pointsValues <- merge(pointID,pollinationPointsSumAbundance)
head(pointsValues)
nrow(pointsValues)

### extract raster data by points
rasValue=raster::extract(pollinationProj, pointsValues)
head(rasValue)
combinePointValue=as.data.frame(cbind(pointsValues,rasValue))
head(combinePointValue)
names(combinePointValue)[(ncol(combinePointValue)-2)] <- "pollinationModelled"
sum(is.na(combinePointValue))
sum(is.na(combinePointValue))
combinePointValue <- na.omit(combinePointValue)

# R2
mod <- lm(pollinationModelled~Abundnc,combinePointValue)
dataQualityR2 <- round(summary(mod)$r.squared,4)
## -> Decision III: quality might be rather low, but still accept dataset because of lacking alternatives?

########################## STEP 4: DATA PROCESSING AND ANALYSIS
# combine pollination and yield data  to table
outputTable <- cbind(as.data.frame(yieldRapeseed),as.data.frame(pollinationRes),as.data.frame(irrigationRapeseed)) 
names(outputTable) <- c("yieldRapeseed","pollination","irrigationRapeseed")
# remove 0 yields and NAs
outputTableFinal <- outputTable[which(outputTable$yieldRapeseed>0&!is.na(outputTable$pollination)),] 
head(outputTableFinal) ## this would be the DATA OUTPUT!
write.csv(outputTableFinal,"myOutputTable.csv")

# model rapeseed yield
modelRapeseed <- lm(yieldRapeseed~pollination+irrigationRapeseed,data=outputTableFinal) ## this would be the MODEL OUTPUT!
save(modelRapeseed,file="modelRapeseed.RData")

########################## STEP 5: ADD DATA TO CKAN

############ STEP 5.1: ADD NEW DATA QUALITY INFORMATION TO POLLINATION DATASET
pollination_metadata[which(names(pollination_metadata) %in% c("relationships_as_object", "relationships_as_subject", "resources", "tags", "groups", "organization"))] <- NULL
pollination_metadata$quality_metrics=paste0("{\"https://geokur-dmp.geo.tu-dresden.de/quality-register#QuantitativeAttributeAccuracyasCoefficientofDetermination\":{\"label\":\"Quantitative Attribute Accuracy as Coefficient of Determination (R²)\",\"values\":{\"value of quality metric\":\"",
                                            dataQualityR2,
                                            "\",\"ground truth dataset\":\"\",\"confidence term\":\"\",\"confidence value\":\"\",\"thematic representativity\":\"\",\"spatial representativity\":\"\",\"temporal representativity\":\"\",\"name of quality source\":\"\",\"type of quality source\":\"\",\"link to quality source\":\"\"}}}")
pollination_metadata[which(names(pollination_metadata) %in% c("relationships_as_object", "relationships_as_subject", "resources", "tags", "groups", "organization"))] <- NULL
package_patch(pollination_metadata)


############ STEP 5.2: UPLOAD NEWLY PRODUCED DATASETS TO CKAN
## output table
# metadata
output_dataset <- package_create(
  extras = c(
    name = "output_dataset",
    title = "Output dataset",
    description = "",
    owner_org = "ufz",
    contact_name = "lukas",
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
    contact_name = "lukas",
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
                      

## model
# metadata
model_rapeseed_output_metadata <- package_create(
  extras = c(
    name = "model_rapeseed_output",
    title = "Model rapeseed output",
    # conforms_to = "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    owner_org = "ufz",
    contact_name = "lukas",
    # theme = "https://inspire.ec.europa.eu/theme/au",
    was_derived_from = paste0(dataset_base_url, output_dataset$id)
  )
)
# model_rapeseed_output_metadata <- package_show("model_rapeseed_output")
# package_delete(id = model_rapeseed_output_metadata$name)

# dataset
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
    contact_name = "lukas",
    used = paste0(dataset_base_url, output_dataset$id),
    generated = paste0(dataset_base_url, model_rapeseed_output_metadata$id)
    # category = "geokur:Selection"
  )
)
# model_rapeseed_metadata <- package_show("model_rapeseed")
# package_delete(id = model_rapeseed_metadata$name)

rm(list=ls())
