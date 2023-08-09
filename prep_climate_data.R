library("sf")
library("tmap")
library("tmaptools")
library("leaflet")
library(ggplot2)
library(tidyverse)
library(rgdal)
library(raster)
library(rdwd)
library(stringr)
library(RColorBrewer)
library(Hmisc)
library(parallel)
library(pbapply)
library(data.table)
library(rgdal)

#### Function to retrieve NUTS3 DWD data ####
nuts3_clim_extr <- function( data_list ){
dl <- data_list
names(dl) <- years

for (i in years) {
  proj4string(dl[[paste0(i)]]) <- 
CRS( "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel + 
   +no_defs")
}

# st_crs(DEshape)
# crs(precip[[1]]) 
dl_vals <- list()

for (i in years) {
dl_vals[[paste0(i)]] <- extract(dl[[paste0(i)]], DEshape)
}

dl_out <- list()
for (i in years) {
dl_out[[paste0(i)]] <- lapply(dl_vals[[paste0(i)]], FUN=mean,na.rm=TRUE) %>% 
 unlist
}

df_dl <- do.call(cbind, dl_out) %>% as.data.frame
df_dl$id <- DEshape@data$FID

df_dl <- gather(df_dl, key = "year", value = "value", -one_of("id"))

 result <- df_dl
 return(result)          
}

#### DWD data 2004-2016 ####
data("gridIndex")

dwd_raster_extr <- function( index_name ){
index <- grep("annual", gridIndex, value=TRUE) %>%
 grep(index_name, ., value=TRUE) %>%
 grep(paste (1961:2019, "", sep="|", collapse="") %>% str_sub(end=-2), .,
      value=TRUE)

index_files <- dataDWD(index, base=gridbase, joinbf=TRUE, read=FALSE) 
index_raster <- readDWD(index_files, quiet=TRUE)

return(index_raster)
}

hot_days <- dwd_raster_extr("hot_days")
drought_index <- dwd_raster_extr("drought_index")
precipGE30mm_days <- dwd_raster_extr("precipGE30mm_days")
precipGE20mm_days <- dwd_raster_extr("precipGE20mm_days")
precipitation <- dwd_raster_extr("precipitation")[-1]
air_temperature_mean <- dwd_raster_extr("air_temperature_mean")[-1]

# saveRDS(list(hot_days, drought_index, precipGE30mm_days,
#         precipGE20mm_days, air_temperature_mean, precipitation),
#         "backup_weather2.rds")
# 
# 
# backup2 <- readRDS("backup_weather2.rds")
# names(backup2) <- c("hot_days", "drought_index", "precipGE30mm_days",
#                    "precipGE20mm_days", "air_temperature_mean", "precipitation") 
                   
# list2env(backup2, envir = .GlobalEnv)

#### Load shape data ####
shape <- readOGR("plz_spatial/plz-3stellig.shp")

## right projection
shape <- spTransform(shape, CRS(
"+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel + 
   +no_defs"))

## subset only relevant zip codes
shape <- shape[startsWith(shape$plz, "63") | 
                startsWith(shape$plz, "8") | 
                startsWith(shape$plz, "9"),]
plot(shape)


dat <- readRDS("data_field/data_calc.rds")
shape5 <- readOGR("plz_spatial/plz-gebiete.shp")

## right projection
shape5 <- spTransform(shape5, CRS(
"+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel + 
   +no_defs"))

shape5 <- shape5[ which(shape5@data$plz %in% 
                          dat$location[which(nchar(dat$location)==5)]),]

## collect all raster fields within PLZ boundaries
extract_zip <- function(list, shape){
 ul <- names(list) %>% startsWith("annual") %>% sum

 list_plz_ann <- pblapply(1:ul, function(i)
  extract(list[[i]], shape), cl = cl)
 
 list_plz_ann_mean <- lapply(1:ul, function(i)
  sapply(list_plz_ann[[i]], FUN=mean,na.rm=TRUE))
 
 df <- cbind( shape@data$plz, do.call(cbind, list_plz_ann_mean)) %>%
 as.data.frame
 
 colnames(df)[1] <- "plz"
 
 df <- setDT(df)[,lapply(.SD,mean,na.rm=TRUE),by=plz]
 
 result <- list(list_plz_ann, list_plz_ann_mean, df)
 return(result)
}

cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
 library(raster)
 library(pbapply)
 library(dplyr)})

clusterExport(cl, c("precipitation", "hot_days", "drought_index", 
                    "precipGE30mm_days", "precipGE20mm_days", 
                    "air_temperature_mean", "shape", "shape5"))

res_hot_days <- extract_zip(hot_days, shape)
res_drought_index <- extract_zip(drought_index, shape)
res_precipGE30mm_days <- extract_zip(precipGE30mm_days, shape)
res_precipGE20mm_days <- extract_zip(precipGE20mm_days, shape)
res_temp <- extract_zip(air_temperature_mean, shape)
res_precip <- extract_zip(precipitation, shape)

saveRDS(list(res_hot_days, res_drought_index, res_precipGE30mm_days,
        res_precipGE20mm_days, res_temp, res_precip),
        "backup_weather.rds")

df_zip_drought <- setDT( 
 cbind( shape@data,do.call(cbind, res_drought_index[[2]])))[
  ,lapply(.SD,mean,na.rm=TRUE),by=plz]

colnames(df_zip_drought) <- c("zip", paste0("drought_index",1995:2019))

backup <- readRDS("backup_weather.rds")
names(backup) <- c("res_hot_days", "res_drought_index", "res_precipGE30mm_days", 
                   "res_precipGE20mm_days", "res_temp", "res_precip") 
                   
list2env(backup, envir = .GlobalEnv)

## Function to prepare data
prep_df <- function(df){
  setDT( cbind( shape@data,do.call(cbind, df[[2]])))[
    ,lapply(.SD,mean,na.rm=TRUE),by=plz]
}

df_hot_days <- prep_df(res_hot_days)
df_drought_index <- prep_df(res_drought_index)
df_precipGE30mm_days <- prep_df(res_precipGE30mm_days)
df_precipGE20mm_days <- prep_df(res_precipGE20mm_days)
df_temp <- prep_df(res_temp)
df_precip <- prep_df(res_precip)

# solve scaling issue
# hot days
for (i in 1:ncol(df_hot_days)) {
    if (is.numeric(df_hot_days[[i]])) 
      df_hot_days[[i]] = round(df_hot_days[[i]] * 10, 0)
}

# precipitation
for (i in 1:ncol(df_precip)) {
    if (is.numeric(df_precip[[i]])) 
      df_precip[[i]] = df_precip[[i]] * 10
}

# precipGE30mm
for (i in 1:ncol(df_precipGE30mm_days)) {
    if (is.numeric(df_precipGE30mm_days[[i]])) 
      df_precipGE30mm_days[[i]] = round(df_precipGE30mm_days[[i]] * 10, 0)
}

# precipGE20mm
for (i in 1:ncol(df_precipGE20mm_days)) {
    if (is.numeric(df_precipGE20mm_days[[i]])) 
      df_precipGE20mm_days[[i]] = round(df_precipGE20mm_days[[i]] * 10, 0)
}

# colnames
colnames(df_hot_days) <- c("zip", paste0("hot_days",1961:2019))
colnames(df_drought_index) <- c("zip", paste0("drought_index",1995:2019))
colnames(df_precipGE30mm_days) <- c("zip", paste0("precipGE30mm_days",1961:2019))
colnames(df_precipGE20mm_days) <- c("zip", paste0("precipGE20mm_days",1961:2019))
colnames(df_temp) <- c("zip", paste0("temp",1961:2019))
colnames(df_precip) <- c("zip", paste0("precip",1961:2019))

#### Dry days data ####
raster_dd <- stack("./data/EU_dry_days.grd")

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
 library(raster)
 library(pbapply)
 library(dplyr)})

clusterExport(cl, c("raster_dd", "shape", "shape5"))

list_plz_dd <- pblapply(1960:2019, function(i)
 extract(raster_dd[[paste0("X", i)]], shape), cl = cl)

list_plz_dd5 <- pblapply(1960:2019, function(i)
 extract(raster_dd[[paste0("X", i)]], shape5), cl = cl)

list_plz_dd_ann <- lapply(1:length(1960:2019), function(i)
  sapply(list_plz_dd[[i]], FUN=mean,na.rm=TRUE))

list_plz_dd_ann5 <- lapply(1:length(1960:2019), function(i)
  sapply(list_plz_dd5[[i]], FUN=mean,na.rm=TRUE))

df_dry_days <- cbind( shape@data, do.call(cbind, list_plz_dd_ann)) %>%
  as.data.frame

df_dry_days5 <- cbind( shape5@data, do.call(cbind, list_plz_dd_ann5)) %>%
  as.data.table %>% .[,-2]

df_dry_days <- setDT(df_dry_days)[,lapply(.SD,mean,na.rm=TRUE),by=plz]

colnames(df_dry_days) <- c("zip", paste0("dry_days",1961:2020))
colnames(df_dry_days5) <- c("zip", paste0("dry_days",1961:2020)) 
df_hot_days5[,-1] <- sapply(df_hot_days5[,-1], as.numeric) 

df_dry_days <- setDT(rbind(df_dry_days, df_dry_days5))

#### Growing days data (between 10 and 30 degrees)####

## GDD wheat https://www.ars.usda.gov/ARSUserFiles/20200000/White%20et%20al.%20FCR2012.pdf 
## GDD corn https://onlinelibrary.wiley.com/doi/epdf/10.1111/ajae.12157 

# T base 5; tmax: 35

## Load data
EU_link <- "C:/Users/ge36raw/Documents/Forschung/weather_tfp_nexus/calc/data/weather/"

raster_Tmin <- stack(paste0(EU_link, "tn_ens_mean_0.1deg_reg_v22.0e.nc"))
raster_Tmax <- stack(paste0(EU_link, "tx_ens_mean_0.1deg_reg_v22.0e.nc"))

years <- 1960:2019
growing_season <- paste0(rep(seq(1960,2019), each=8), ".", c(rep(0,7), 1) , c(3:9, 0), ".")

raster_Tmax <- raster_Tmax[[tidyselect::vars_select(
  names(raster_Tmax), 
  contains(growing_season))]]

raster_Tmin <- raster_Tmin[[tidyselect::vars_select(
  names(raster_Tmin), 
  contains(growing_season))]]

shape5$note <- NULL
shapeAll <- rbind(shape, shape5)
shape1 <- spTransform(shapeAll, CRS(proj4string(raster_Tmax)))


raster_Tmax <- crop(raster_Tmax, shape1)
raster_Tmin <- crop(raster_Tmin, shape1)
  
list_Tmax <- lapply( years %>% as.character(), function(i) 
  raster_Tmax[[tidyselect::vars_select(
    names(raster_Tmax), 
    contains(i))]])

list_Tmin <- lapply( years %>% as.character(), function(i) 
  raster_Tmin[[tidyselect::vars_select(
    names(raster_Tmin), 
    contains(i))]])

names(list_Tmax) <- names(list_Tmin) <- years

cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
 library(raster)
 library(pbapply)
 library(dplyr)})

clusterExport(cl, c("years", "list_Tmax", "list_Tmin", "shape1"))

list_plz_daily_Tmax <- pblapply(years %>% as.character(), function(i)
  extract(list_Tmax[[i]], shape1))

list_plz_daily_Tmin <- pblapply(years %>% as.character(), function(i)
  extract(list_Tmin[[i]], shape))

names(list_plz_daily_Tmax) <- names(list_plz_daily_Tmin) <- years

list_df_plz_daily_Tmax <- pblapply(years %>% as.character(), function(x)
  t(sapply(1:length(list_plz_daily_Tmax[[x]]), function(i)
    colMeans(list_plz_daily_Tmax[[x]][[i]]))) %>%
    as.data.frame)

list_df_plz_daily_Tmin <- pblapply(years %>% as.character(), function(x)
  t(sapply(1:length(list_plz_daily_Tmin[[x]]), function(i)
    colMeans(list_plz_daily_Tmin[[x]][[i]]))) %>%
    as.data.frame)

names(list_df_plz_daily_Tmax) <- names(list_df_plz_daily_Tmin) <- years

list_plz_daily_gdd <- 
  pblapply(years %>% as.character(), function(x)
    sapply(2:ncol(list_df_plz_daily_Tmin[[x]]), function(y)
      sapply( 1:nrow(list_df_plz_daily_Tmin[[x]]), function(z)
        pollen::gdd(tmax = list_df_plz_daily_Tmax[[x]][z,y],
                    tmin = list_df_plz_daily_Tmin[[x]][z,y],
                    tbase = 5,
                    tbase_max = 35))))

names(list_plz_daily_gdd) <- years

lapply(as.character(years), function(i) rowSums(list_plz_daily_gdd[[i]]))

df_gdd <- cbind(shape1@data, sapply(list_plz_daily_gdd, FUN=rowSums)) %>%
  as.data.frame

colnames(df_gdd) <-  c("zip", paste0("gdd",colnames(df_gdd)[-1]))


#### 5-digit observations ####
extract_zip5 <- function(list, shape){
 ul <- names(list) %>% startsWith("annual") %>% sum

 list_plz_ann <- pblapply(1:ul, function(i)
  extract(list[[i]], shape), cl = cl)
 
 list_plz_ann_mean <- lapply(1:ul, function(i)
  sapply(list_plz_ann[[i]], FUN=mean,na.rm=TRUE))
 
 df <- cbind( shape@data$plz, do.call(cbind, list_plz_ann_mean)) %>%
 as.data.frame
 
 colnames(df)[1] <- "plz"
 
 result <- list(list_plz_ann, list_plz_ann_mean, df)
 return(result)
}

clusterExport(cl, c( "shape5"))

res_hot_days5 <- extract_zip5(hot_days, shape5)
res_drought_index5 <- extract_zip5(drought_index, shape5)
res_precipGE30mm_days5 <- extract_zip5(precipGE30mm_days, shape5)
res_precipGE20mm_days5 <- extract_zip5(precipGE20mm_days, shape5)
res_temp5 <- extract_zip5(air_temperature_mean, shape5)
res_precip5 <- extract_zip5(precipitation, shape5)

saveRDS(list(res_hot_days5, res_drought_index5, res_precipGE30mm_days5,
        res_precipGE20mm_days5, res_temp5, res_precip5),
        "backup_weather5.rds")

df_hot_days5 <- res_hot_days5[[3]]
df_drought_index5 <- res_drought_index5[[3]]
df_precipGE30mm_days5 <- res_precipGE30mm_days5[[3]]
df_precipGE20mm_days5 <- res_precipGE20mm_days5[[3]]
df_temp5 <- res_temp5[[3]]
df_precip5 <- res_precip5[[3]]

df_hot_days5[,-1] <- sapply(df_hot_days5[,-1], as.numeric) * 10
df_precipGE30mm_days5[,-1] <- sapply(df_precipGE30mm_days5[,-1], as.numeric) * 10
df_precipGE20mm_days5[,-1] <- sapply(df_precipGE20mm_days5[,-1], as.numeric) * 10
df_precip5[,-1] <- sapply(df_precip5[,-1], as.numeric) * 10
df_drought_index5[,-1] <- sapply(df_drought_index5[,-1], as.numeric)
df_temp5[,-1] <- sapply(df_temp5[,-1], as.numeric)

colnames(df_hot_days5) <- c("zip", paste0("hot_days",1961:2019))
colnames(df_drought_index5) <- c("zip", paste0("drought_index",1995:2019))
colnames(df_precipGE30mm_days5) <- c("zip", paste0("precipGE30mm_days",1961:2019))
colnames(df_precipGE20mm_days5) <- c("zip", paste0("precipGE20mm_days",1961:2019))
colnames(df_temp5) <- c("zip", paste0("temp",1961:2019))
colnames(df_precip5) <- c("zip", paste0("precip",1961:2019))

df_hot_days <- setDT(rbind( df_hot_days, df_hot_days5))
df_drought_index <-  setDT(rbind( df_drought_index, df_drought_index5))
df_precipGE30mm_days <-  setDT(rbind( df_precipGE30mm_days, df_precipGE30mm_days5))
df_precipGE20mm_days <-  setDT(rbind( df_precipGE20mm_days, df_precipGE20mm_days5))
df_temp <-  setDT(rbind( df_temp, df_temp5))
df_precip <-   setDT(rbind( df_precip, df_precip5)) %>% as.data.frame

#### DF weather indicators ####
df_list <- list(df_hot_days, df_drought_index, df_precipGE30mm_days, 
                df_precipGE20mm_days, df_temp, df_precip, df_dry_days)

# saveRDS(df_list, "backup_weather_df_list.rds")
df_list <- readRDS("backup_weather_df_list.rds")

df_list_names <- c("hot_days", "drought_index", "precipGE30mm_days", 
                "precipGE20mm_days", "temp", "precip", "dry_days",
                "gdd")

names(df_list) <- df_list_names

df_index <- data.frame(zip=df_list[[1]][,1])

for (i in df_list_names) {
  df_index[[paste0(i, "_mean_20")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2000:2019)) %>%
    rowMeans

  df_index[[paste0(i, "_mean_20_3")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2000:2016)) %>%
    rowMeans
  
  df_index[[paste0(i, "_mean_15")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2005:2019)) %>%
    rowMeans
    
  df_index[[paste0(i, "_mean_10")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2010:2019)) %>%
    rowMeans
  
    df_index[[paste0(i, "_mean_10_3")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2010:2016)) %>%
    rowMeans
  
    df_index[[paste0(i, "_mean_5")]] <- df_list[[i]] %>%
      dplyr::select(paste0(i, 2015:2019)) %>%
      rowMeans
   
  df_index[[paste0(i, "_mean_3")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2017:2019)) %>%
    rowMeans
  
  df_index[[paste0(i, "_2019")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 2019)) %>%
    rowMeans
  
  df_index[[paste0(i, "_ann_growth_20_rel")]] <- 
    ((df_list[[i]][[paste0(i, "2019")]] / 
       df_list[[i]][[paste0(i, "1999")]])^(1/20) - 1) *100
  
  df_index[[paste0(i, "_ann_growth_10_rel")]] <- 
    ((df_list[[i]][[paste0(i, "2019")]] / 
        df_list[[i]][[paste0(i, "2010")]])^(1/10) - 1) *100
  
    df_index[[paste0(i, "_ann_growth_20_abs")]] <- 
    (df_list[[i]][[paste0(i, "2019")]] - 
       df_list[[i]][[paste0(i, "1999")]])/20
  
  df_index[[paste0(i, "_ann_growth_10_abs")]] <- 
    (df_list[[i]][[paste0(i, "2019")]] - 
        df_list[[i]][[paste0(i, "2010")]])/10
  
    df_index[[paste0(i, "_ann_growth_5_abs")]] <- 
    (df_list[[i]][[paste0(i, "2019")]] - 
        df_list[[i]][[paste0(i, "2015")]])/5
    
      df_index[[paste0(i, "_ann_growth_15_abs")]] <- 
    (df_list[[i]][[paste0(i, "2019")]] - 
        df_list[[i]][[paste0(i, "2005")]])/15
}


for (i in df_list_names[-c(2,8)]) {
  df_index[[paste0(i, "_normal")]] <- df_list[[i]][,2:31] %>% rowMeans
}

for (i in df_list_names[-c(2,8)]) {
  df_index[[paste0(i, "_mean_25")]] <- df_list[[i]] %>%
    dplyr::select(paste0(i, 1994:2019)) %>%
    rowMeans
}

for (i in df_list_names[-c(2,8)]) {
  df_index[[paste0(i, "_anomly_25")]] <- 
    df_index[[paste0(i, "_mean_25")]] - df_index[[paste0(i, "_normal")]]
  
  df_index[[paste0(i, "_anomly_20")]] <- 
    df_index[[paste0(i, "_mean_20")]] - df_index[[paste0(i, "_normal")]]
  
   df_index[[paste0(i, "_anomly_15")]] <- 
    df_index[[paste0(i, "_mean_15")]] - df_index[[paste0(i, "_normal")]]
                                                 
  df_index[[paste0(i, "_anomly_10")]] <- 
    df_index[[paste0(i, "_mean_10")]] - df_index[[paste0(i, "_normal")]]
  
  df_index[[paste0(i, "_anomly_5")]] <- 
    df_index[[paste0(i, "_mean_5")]] - df_index[[paste0(i, "_normal")]]
  
  df_index[[paste0(i, "_anomly_3")]] <- 
    df_index[[paste0(i, "_mean_3")]] - df_index[[paste0(i, "_normal")]]
  
  df_index[[paste0(i, "_anomly_2019")]] <- 
    df_index[[paste0(i, "_2019")]] - df_index[[paste0(i, "_normal")]]
}

saveRDS(df_index, "data_field/data_climate_indicators.rds")
