#### Load R Packages ####
library( miscTools )
library( readr )
library( dplyr )
library( beepr )
library( data.table )
library( hablar )
library( EnvStats )
library( Hmisc )
library(tidyverse)

#### 1 Load raw data ####
data_raw <- read_delim("data_field/data_project_raw.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

block_colnames <- c("ID", paste0("q", 1:12), 
                    paste0("ana_", c("DB", "DBVar", "ND", "AU", "OVF", "all")))

#### +Isolate data for  DCE blocks ####
block1_start <- which(colnames(data_raw) == "q1")
block1_subset <- c(1, block1_start:(block1_start+17))
dat_block1 <- data_raw[,block1_subset] %>% drop_na()

block2_start <- which(colnames(data_raw) == "q1_b2")
block2_subset <- c(1, block2_start:(block2_start+17))
dat_block2 <- data_raw[,block2_subset] %>% drop_na()

block3_start <- which(colnames(data_raw) == "q1_b3")
block3_subset <- c(1, block3_start:(block3_start+17))
dat_block3 <- data_raw[,block3_subset] %>% drop_na()

block_list <- list( dat_block1, dat_block2, dat_block3)
names(block_list) <- c("dat_block1", "dat_block2", "dat_block3")

for (i in 1:3) {
 colnames(block_list[[i]]) <- block_colnames
 
 block_list[[i]]$BLOCK <- i
}

list2env(block_list, envir = .GlobalEnv)

#### + Isolate farm info ####
fi_start <- which(colnames(data_raw) == "conventional_dummy")
fi_end <- which(colnames(data_raw) == "q1")-3
farm_info_subset <- c(1, fi_start:fi_end)
dat_farm_info <- data_raw[,farm_info_subset] %>%
        rename( FullTime = v_416,
                PartTime = v_417,
                FarmType = v_422,
                qLabFull = v_429,
                qLabSeason = v_431,
                age = v_438,
                educNone = v_432,
                educLehre = v_433,
                educMeister = v_434,
                educUni = v_435,
                educPhD = v_436,
                educOther = v_437,
                AESdummy = v_393,
                conv_and_organic = part_time_dummy)

#### ++ PLausibility land #####
sapply(dat_farm_info,is.numeric)

## Check plausibility of responses wrt land
dat_farm_info$qLand <- dat_farm_info$qLand %>% 
 gsub(",", ".",.) %>% 
 gsub("o", 0,.) %>% as.numeric()

dat_farm_info$share_forest <- gsub(",", ".", dat_farm_info$share_forest) %>% 
        as.numeric %>%
        ifelse(is.na(.), 0, .)

SumShareLand <- with(dat_farm_info, share_arable + share_grassland + share_forest)
## Fix issues with decimals for some observations
PlausiCheckLand <- which(SumShareLand != 100)
# print(dat_farm_info[PlausiCheckLand,c("qLand", "share_arable", 
#                                 "share_grassland", "share_forest")])


dat_farm_info$share_arable <- dat_farm_info$share_arable %>% 
        str_replace_all("(.{2})", "\\1.") %>% 
        substr(.,1,nchar(.)-1) %>%
        as.numeric

dat_farm_info$share_grassland <- dat_farm_info$share_grassland %>% 
        str_replace_all("(.{2})", "\\1.") %>% 
        substr(.,1,nchar(.)-1) %>%
        as.numeric

SumShareLand <- with(dat_farm_info, share_arable + share_grassland + share_forest)
PlausiCheckLand <- which(SumShareLand != 100 &
                         SumShareLand == dat_farm_info$qLand)
print(dat_farm_info[PlausiCheckLand,c("qLand", "share_arable", 
                                "share_grassland", "share_forest")])

for (i in PlausiCheckLand) {
 dat_farm_info$share_arable[i] <- with(dat_farm_info, share_arable[i]/qLand[i]) * 100
 dat_farm_info$share_grassland[i] <- with(dat_farm_info, share_grassland[i]/qLand[i]) * 100
 dat_farm_info$share_forest[i] <- with(dat_farm_info, share_forest[i]/qLand[i]) * 100
}

# SumShareLand <- with(dat_farm_info, share_arable + share_grassland + share_forest)
# PlausiCheckLand <- which(SumShareLand < 90 | SumShareLand > 110)

PlausiCheckLand <- which(is.na(dat_farm_info$qLand))
print(dat_farm_info[PlausiCheckLand,c("qLand", "share_arable",
                                "share_grassland", "share_forest")])

dat_farm_info$share_rented_land <- 
        gsub(",", ".", dat_farm_info$share_rented_land) %>% 
        as.numeric %>%
        ifelse(is.na(.), 0, .)

#### ++ Plausibility labor #####
dat_farm_info$qLabFull <- 
        gsub(",", ".", dat_farm_info$qLabFull) %>% 
        as.numeric

ifelse(is.na(dat_farm_info$qLabFull) & dat_farm_info$v_428 == 1 &
             dat_farm_info$FullTime == 1, 1, dat_farm_info$qLabFull) 


ifelse(is.na(dat_farm_info$qLabFull) & dat_farm_info$v_428 == 1 &
             dat_farm_info$FullTime == 0, 0.5, dat_farm_info$qLabFull) 

print(dat_farm_info[is.na(dat_farm_info$qLabFull),])
colnames(dat_farm_info)[1] <- "ID"

#### ++ Plausibility location ####
# which(nchar(dat_farm_info$age) > 2)
dat_farm_info$age[which(nchar(dat_farm_info$age) > 2)] <- c(32, 59)
dat_farm_info$age <- as.numeric(dat_farm_info$age)
# which(nchar(dat_farm_info$age) > 2)

#### + Isolate personal questions ####
sec3_start <- which(colnames(data_raw) == "sa_risk_attitude")
sec3_end <- which(colnames(data_raw) == "fut_drop_out")
sec3_subset <- c(1, sec3_start:sec3_end)
dat_sec3 <- data_raw[,sec3_subset]

fut_cols <- dat_sec3 %>% dplyr::select(starts_with("fut")) %>% colnames
dat_sec3[,fut_cols] <- dat_sec3[,fut_cols] - 5

for (i in fut_cols) {
        for (j in 1:nrow(dat_sec3)) {
                dat_sec3[j,i] <- ifelse(dat_sec3[j,i] ==-5, NA, dat_sec3[j,i])
        }
}

dat_sec3$ID <- dat_sec3$lfdn
#### 2 Plausibility Checks ####
#### + Time per page ####
data_plausi <- read_delim("data_field/data_project_plausi.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE) %>%
 dplyr::select(c(1, which(str_detect(colnames(.), "rts"))))

colnames(data_plausi) <- c( "ID", paste0("rts_", c("farm_info", "personal_att", 
                                                   "block1", "block2", "block3",
                                                   "startpage", "filter")))

data_plausi$durationComplete <- data_plausi$rts_personal_att / 60
data_plausi$durationFarmInfo <- data_plausi$rts_farm_info / 60

data_plausi$durationDCE <- with(data_plausi, 
                                ifelse(rts_block2 == 0 & rts_block3 == 0,
                                       rts_block1 - rts_farm_info, NA))
data_plausi$durationDCE <- with(data_plausi, 
                                ifelse(rts_block1 == 0 & rts_block3 == 0,
                                       rts_block2 - rts_farm_info, durationDCE))
data_plausi$durationDCE <- with(data_plausi, 
                                ifelse(rts_block1 == 0 & rts_block2 == 0,
                                       rts_block3 - rts_farm_info, durationDCE)) / 60
                                                
        
data_time_and_dce1 <- left_join(dat_block1, data_plausi, by = "ID")
data_time_and_dce2 <- left_join(dat_block2, data_plausi, by = "ID")
data_time_and_dce3 <- left_join(dat_block3, data_plausi, by = "ID")

#### 3 Generate data ####
datDCE <- rbind(data_time_and_dce1, data_time_and_dce2, data_time_and_dce3)
datFIDCE <- left_join(datDCE, dat_farm_info, by = "ID")
dat_full <- left_join(datFIDCE, dat_sec3, by = "ID")

dat_full$zip <- ifelse(nchar(dat_full$location) == 5, 
                       dat_full$location, 
                       dat_full$location %>% substr(1, 3))
dat_full$zip[46] <- "896" 

## Check if there is variation in the DCE responses
test <- datDCE %>% dplyr::select(starts_with("q"))

# Only 1s: SRC
plausi111 <- sapply(1:nrow(datDCE), function(i) 
        isTRUE(all.equal(test[i,], rep(1, 12), check.attributes = F)))

#Only 2s: Alley-Cropping
plausi222 <- sapply(1:nrow(datDCE), function(i) 
        isTRUE(all.equal(test[i,], rep(2, 12), check.attributes = F)))

# Only 3s: Status Quo
plausi333 <- sapply(1:nrow(datDCE), function(i) 
        isTRUE(all.equal(test[i,], rep(3, 12), check.attributes = F)))

dat_full[plausi333,]

dat_full[dat_full$ID %in% dat_full[plausi222,]$ID,] %>% 
        dplyr::select(durationDCE) %>% 
        summary

saveRDS(dat_full, "data_field/data_clean.rds")

df_climate <- readRDS("data_field/data_climate_indicators.rds")

dat_full <- left_join(dat_full, df_climate, by = "zip")

#### Prepare data for estimation ####

## Climate change hazard index ##
dat_full$cc_index <- dat_full %>% dplyr::select(starts_with(c("ci", "cc"))) %>%
        dplyr::select(-c("ci_other")) %>%
        mutate_all(function(x) ifelse(x==1,1,0)) %>%
        rowSums

dat_full$yield_high <- ifelse(
        dat_full$yield_potential == 4 |dat_full$yield_potential == 5, 1,0)

dat_full$yield_low <- ifelse(dat_full$yield_potential == 1 | 
                                     dat_full$yield_potential == 2, 1,0)

dat_full$yield_medium <- ifelse(dat_full$yield_potential == 3, 1,0)

dat_full$risk_high <- ifelse(dat_full$sa_risk_attitude == 4 | 
                                     dat_full$sa_risk_attitude == 5, 1,0)
dat_full$risk_low <- ifelse(dat_full$sa_risk_attitude == 1 | 
                                    dat_full$sa_risk_attitude == 2, 1,0)
dat_full$risk_medium <- ifelse(dat_full$sa_risk_attitude == 3, 1,0)

dat_full$loc_dummy <- ifelse(substr(dat_full$location, 1, 1) == 8, 1, 0)

dat_full$food_focus <- with(dat_full, ifelse(sa_focus_food>3, 1, 0))

dat_full$ci_dummy <- ifelse(dat_full %>%
                          dplyr::select(starts_with("ci")) %>%
                          dplyr::select(-starts_with("ci_other")) %>%
                          rowSums<2, 1, 0)

dat_full$FarmTypeDummy <- with(dat_full, ifelse(FarmType == 1, 1, 0))


#### Center climate data ####
data <- dat_full %>% 
        filter(!is.na(hot_days_2019)) 

saveRDS(data, "data_field/data_calc.rds")