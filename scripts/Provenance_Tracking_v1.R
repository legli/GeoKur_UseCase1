## Provenance tracking for GeoKur use case 1
# Author: Lukas Egli & Stefano Della Chiesa
# Date: 25/05/2021

#---- 
# Required
if (!require("devtools")) install.packages("devtools");library ("devtools")

## PROVENANCE
# Install rdtLite Package to collect provenance as an R script executes 
install_github("End-to-end-provenance/rdtLite", force = TRUE)
library(rdtLite)

prov.run("scripts/useCase1_SDC_rdt_lite_v1.R")

#devtools::install_github("End-to-end-provenance/provSummarizeR")
#library("provSummarizeR")

#prov.summarize("C:/Users/chiesa/AppData/Local/Temp/Rtmp2BtCaA/prov_useCase1_SDC_rdt_lite_v1/prov.json")

## VISUALIZE PROVENANCE WITHIN R
install.packages("provViz")
library("provViz")

# not working
#prov.visualize.file("C:/Users/chiesa/AppData/Local/Temp/Rtmp2BtCaA/prov_useCase1_SDC_rdt_lite_v1/prov.json")
#testdata <- system.file("C:/Users/chiesa/AppData/Local/Temp/Rtmp2BtCaA/prov_useCase1_SDC_rdt_lite_v1/prov", "prov.json", package = "provViz")
#prov.visualize.file(testdata)

# ----
## rdflib can convert JSON-LD fomrat (not PROV-JSON Serialization)
# test is the ProvToolbox might work as standalone external tool https://lucmoreau.github.io/ProvToolbox/ 
#devtools::install_github("ropensci/rdflib")
#library(rdflib)


#########
# install.packages("installr")
# library(installr)
# install.java(
#   version = 11,
#   page_with_download_url = "http://jdk.java.net/java-se-ri/",
#   path = "C:/java"
# )
