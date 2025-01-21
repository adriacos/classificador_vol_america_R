library(odbc)
library(RODBC)
# library(stringr)

url <- "https://creaf-my.sharepoint.com/:i:/g/personal/a_cos_creaf_uab_cat/Ec1yLFbOks5Lv00KOzH4qgIBdNNvmrJ2dBS9-w4YfKBL7Q?e=ZeBMzp"
destfile <- "./rasters/BCN_GIR_LLE.tif"
download.file(url, destfile)


postgresqlCon <- dbConnect(odbc::odbc(), .connection_string = "Driver={ODBC Driver 18 for SQL Server};Server=95.216.35.146;\nDatabase=ifn;\nUID=ifn_guest;\nPWD=AB2ttf63HXB2PavdbgufpwKX;\nPort=5432;")
