sim_drought <- function(RegBez="091", shock_dur=1, extreme_year=2018,
                        AC_DB=400, 
                        AC_DBVar=30,
                        AC_Duruse=16, 
                        AC_AES=200, 
                        AC_NoGreen=1,
                        SRC_DB=400,
                        SRC_DBVar=30,
                        SRC_Duruse=16,
                        SRC_AES=200,
                        SRC_NoGreen=1,
                        
                        SQ_DB=400, 
                        SQ_DBVar=15,
                        SQ_Duruse=3) {
  oldw <- getOption("warn")
  options(warn = -1)
  
  bRB <- dat1 %>% select(SCH,starts_with("b_"))

   out <- split( bRB , f = dat1$SCH )
  
   out <- lapply(out, function(i) i %>% select(-c(1)))
  out[["09"]] <- bRB %>% select(-c(1))
  
  cnames_out <- c("b_SRC", "b_AC", "b_DB", "b_DBVar", "b_ND", "b_AU", "b_No",
                  "b_rain_1_3_c1", "b_rain_1_3_c2",  "b_rain_4_10_c1",
                  "b_rain_4_10_c2", 
                  "b_temp_1_3_c1", "b_temp_1_3_c2",  "b_temp_4_10_c1",
                  "b_temp_4_10_c2",
                  "b_dd_1_3_c1", "b_dd_1_3_c2", "b_dd_4_10_c1", "b_dd_4_10_c2",
                  "b_r20_1_3_c1", "b_r20_1_3_c2", "b_r20_4_10_c1",
                  "b_r20_4_10_c2",
                  "b_hd_1_3_c1", "b_hd_1_3_c2", "b_hd_4_10_c1", "b_hd_4_10_c2")
  
  options(warn = oldw)
  
  c_1_3 <- dat %>% select(ends_with("1_3")) %>% colMeans()
  c_4_10 <- dat %>% select(ends_with("4_10")) %>% colMeans()
  
  SRC_meanX <- c( 1, 0, SRC_DB, SRC_DBVar, SRC_Duruse, SRC_AES, SRC_NoGreen)
  AC_meanX <- c( 0, 1, AC_DB, AC_DBVar, AC_Duruse, AC_AES, AC_NoGreen)
  SQ_meanX <- c( 0, 0, SQ_DB, SQ_DBVar, SQ_Duruse, 0, 1)
  meanX <- rbind(SRC_meanX,AC_meanX,SQ_meanX)
  
  ALT <- c("Short Rotation Coppice",
           "Alley Cropping",
           "Status Quo")
   
  arrayXsim <- array(0, c(3, 27, 11),
                     dimnames = list(
                       ALT,
                       cnames_out, #colnames(out[[RegBez]]),
                       0:10))
  
  weather_base_c_1_3 <- 
    (df_sim %>% dplyr::filter(ID==RegBez) %>% select(ends_with("_base30")) - c_1_3) %>% 
    unlist
  
  weather_base_c_4_10 <- 
    (df_sim %>% filter(ID==RegBez) %>% select(ends_with("_base30")) - c_4_10) %>% 
    unlist

  if (extreme_year==2018) {
   weather_drought_c_1_3 <- 
    (df_sim %>% filter(ID==RegBez) %>% select(ends_with("drought_year")) - c_1_3) %>% 
    unlist
   
   weather_drought_c_4_10 <- 
    (df_sim %>% filter(ID==RegBez) %>% select(ends_with("drought_year")) - c_4_10) %>% 
    unlist
  } 
  else if (extreme_year==2003){
   weather_drought_c_1_3 <- 
    (df_sim %>% filter(ID==RegBez) %>% 
      select(ends_with("drought_year2003")) - c_1_3) %>% 
    unlist
   
   weather_drought_c_4_10 <- 
    (df_sim %>% filter(ID==RegBez) %>% 
      select(ends_with("drought_year2003")) - c_4_10) %>% 
    unlist
  }
  
  
  ## Assign array
  arrayXsim[1:3,1:7,] <- meanX
  
  if (shock_dur==1) {
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(1, 5:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(1, 5:11)] <- 
      weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(1:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(1:4)] <- 
      weather_base_c_4_10
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(2:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(2:4)] <- 
      1/3 * weather_drought_c_1_3 + 2/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(5:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(5:11)] <- 
      1/7 * weather_drought_c_4_10 + 6/7 *weather_base_c_4_10
  }
  
  else if (shock_dur==2){
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(1, 6:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(1, 6:11)] <- 
      weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(1:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(1:4)] <- 
      weather_base_c_4_10
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(2,5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(2,5)] <- 
      1/3 * weather_drought_c_1_3 + 2/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(3:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(3:4)] <- 
      2/3 * weather_drought_c_1_3 + 1/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(5)] <- 
      1/7 * weather_drought_c_4_10 + 6/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(6:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(6:11)] <- 
      2/7 * weather_drought_c_4_10 + 5/7 *weather_base_c_4_10 
  }
  else if(shock_dur==3){
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(1, 7:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(1, 7:11)] <- 
      weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(1:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(1:4)] <- 
      weather_base_c_4_10
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(2,6)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(2,6)] <- 
      1/3 * weather_drought_c_1_3 + 2/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(3,5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(3,5)] <- 
      2/3 * weather_drought_c_1_3 + 1/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(4)] <- 
       weather_drought_c_1_3 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(5)] <- 
      1/7 * weather_drought_c_4_10 + 6/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(6)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(6)] <- 
      2/7 * weather_drought_c_4_10 + 5/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(6:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(6:11)] <- 
      3/7 * weather_drought_c_4_10 + 4/7 *weather_base_c_4_10  
  } 
  else if(shock_dur==4){
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(1, 8:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(1,8:11)] <- 
      weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(1:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(1:4)] <- 
      weather_base_c_4_10
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(2,7)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(2,7)] <- 
      1/3 * weather_drought_c_1_3 + 2/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(3,6)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(3,6)] <- 
      2/3 * weather_drought_c_1_3 + 1/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(4:5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(4:5)] <- 
       weather_drought_c_1_3 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(5)] <- 
      1/7 * weather_drought_c_4_10 + 6/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(6)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(6)] <- 
      2/7 * weather_drought_c_4_10 + 5/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(7)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(7)] <- 
      3/7 * weather_drought_c_4_10 + 4/7 *weather_base_c_4_10  
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(8:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(8:11)] <- 
      4/7 * weather_drought_c_4_10 + 3/7 *weather_base_c_4_10  
  } 
  else if(shock_dur==5){
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(1, 9:11)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(1,9:11)] <- 
      weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(1:4)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(1:4)] <- 
      weather_base_c_4_10
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(2,8)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(2,8)] <- 
      1/3 * weather_drought_c_1_3 + 2/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(3,7)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(3,7)] <- 
      2/3 * weather_drought_c_1_3 + 1/3 *weather_base_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "1_3_c1"), c(4:6)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "1_3_c2"), c(4:6)] <- 
       weather_drought_c_1_3
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(5)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(5)] <- 
      1/7 * weather_drought_c_4_10 + 6/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(6)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(6)] <- 
      2/7 * weather_drought_c_4_10 + 5/7 *weather_base_c_4_10 
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(7)] <- 
      arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(7)] <- 
      3/7 * weather_drought_c_4_10 + 4/7 *weather_base_c_4_10  
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(8:11)] <- 
     arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(8:11)] <- 
     4/7 * weather_drought_c_4_10 + 3/7 *weather_base_c_4_10  
    
    arrayXsim[1,endsWith(colnames(arrayXsim), "4_10_c1"), c(9:11)] <- 
     arrayXsim[2,endsWith(colnames(arrayXsim), "4_10_c2"), c(9:11)] <- 
     5/7 * weather_drought_c_4_10 + 2/7 *weather_base_c_4_10  
  } 
  
 result <- lapply(1:11, function (y)
   rowMeans(
    do.call(cbind,
            lapply(1:nrow(out[[RegBez]]), function(i)
              exp(arrayXsim[,,y] %*% t(out[[RegBez]][i,])) / 
                sum(exp(arrayXsim[,,y] %*% t(out[[RegBez]][i,])))))))
 
 result1 <- as.data.frame(do.call(rbind, result))
 
 result2 <- tidyr::pivot_longer(
  result1  %>%
   mutate(RegBez=RegBez, year=0:10, p_base=NA), 1:3) %>% 
  rename(ALT=name, p_shock=value)
 
 for (i in ALT) {
  result2$p_base[result2$ALT==i] <- 
   result2$p_shock[result2$ALT==i & result2$year==0]
 }

 result3 <- tidyr::pivot_longer( result2, c("p_base", "p_shock"))

 output <- list(result3, arrayXsim)
 names(output) <- c("res_df", "X_sim")
 # b <- tidyr::pivot_longer(b, 3:4)
 return(result3)
}
