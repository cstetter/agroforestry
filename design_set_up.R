## Setting up a choice design
library(support.CEs)
library(survival)
library(mded)

d_land_use <- Lma.design(
 attribute.names = list(
  DB = c("400", "600", "800"),
  DBVar = c("15", "30"),
  ND = c("16", "20", "24"),
  AU = c("0", "100", "200"),
  Greening = c("Yes","No")),
 nalternatives = 2,
 nblocks = 3,
 row.renames = FALSE, 
 seed = 333)

common_alt <- c(DB = "400", DBVar = "15", ND = "3", AU = "0", Greening = "No")
questionnaire(d_land_use, common = common_alt)

dm_land_use <- make.design.matrix(
choice.experiment.design = d_land_use,
optout = T, # include opt-out option
categorical.attributes = c("Greening"),
continuous.attributes = c("DB", "DBVar", "ND", "AU"),
unlabeled = F,
# common = common_alt
)


## hier bitte anpassen
DB1 <- c( 800, 400, 800, 400, 400, 800, 600, 600,600, 400, 600, 800,
          600,600,400,400,400,600,800,800,800,400,800,600,
          rep(0,12) )  
DBVar1 <- c( 30,30,15,15,30,15,15,15,15,30,30,30,
             15,30,30,30,15,30,15,15,15,15,30,30,
             rep(0,12)) 
ND1 <- c(20,20,16,16,24, 24, 20,24,20,24,16,16,
         20,24,20,24,16,16,24,16,24,20,20,16,
         rep(0,12))
AU1 <- c(0,200,200,0,100,0,200,100,100,200,0,100,
         100,0,200,100,100,200,200,200,0,0,0,100,
         rep(0,12))
Yes1 <- c(1,1,0,1,0,1,0,0,1,0,0,1,
          1,1,1,1,1,0,1,0,0,0,0,0,
          rep(0,12))

DB2 <- c(600,400,600,400,800,400,800,400,600,600,800,800,
         600,600,400,800,600,400,800,600,400,800,400,800,
         rep(0,12))
DBVar2 <- c(30,15,15,15,15,30,15,30,30,30,30,15,
            15,15,15,30,30,15,30,30,15,15,30,30,
            rep(0,12))
ND2 <- c(16,20,24,16,24,24,16,16,24,20,20,20,
         20,24,24,20,16,16,16,20,20,16,24,24,
         rep(0,12))
AU2 <- c(100,200,100,0,100,200,0,100,200,0,200,0,
         100,0,100,200,0,200,0,200,100,200,0,100,
         rep(0,12))
Yes2 <- c(0,1,0,0,1,1,1,0,1,0,1,0,
          0,1,0,0,1,1,1,0,1,0,1,0,
          rep(0,12))


dm_land_use$DB1[dm_land_use$DB1 !=0] <-  DB1
dm_land_use$DBVar1[dm_land_use$DBVar1 !=0] <- DBVar1
dm_land_use$ND1[dm_land_use$ND1 !=0] <- ND1
dm_land_use$AU1[seq(1,108,3)] <- AU1
dm_land_use$No1[seq(1,108,3)] <- ifelse(Yes1==1, 0,1)
dm_land_use$DB2[dm_land_use$DB2 !=0] <- DB2
dm_land_use$DBVar2[dm_land_use$DBVar2 !=0] <- DBVar2
dm_land_use$ND2[dm_land_use$ND2 !=0] <- ND2
dm_land_use$AU2[seq(2,108,3)] <- AU2
dm_land_use$No2[seq(1,108,3)] <- ifelse(Yes2==1, 0,1)

saveRDS(dm_land_use, "data_field/design_matrix.rds")

## dummy coded
dm_land_use_dummy <- make.design.matrix(
choice.experiment.design = d_land_use,
optout = T, # include opt-out option
categorical.attributes = c("Greening", "DBVar", "ND"),
continuous.attributes = c("DB", "AU"),
unlabeled = F,
# common = common_alt
)

dm_land_use_dummy <- cbind(dm_land_use_dummy, 
                           select(dm_land_use, starts_with(c("DBVar", "ND"))))
## hier bitte anpassen
DB1 <- c( 800, 400, 800, 400, 400, 800, 600, 600,600, 400, 600, 800,
          600,600,400,400,400,600,800,800,800,400,800,600,
          rep(0,12) )  
X301 <- c( 1,1,0,0,1,0,0,0,0,1,1,1,0,1,1,1,0,1,0,0,0,0,1,1,rep(0,12)) 

X201 <- c(1,1,0,0,0, 0, 1,0,1,0,0,0, 1,0,1,0,0,0,0,0,0,1,1,0,rep(0,12))

X241 <- c(0,0,0,0,1, 1, 0,1,0,1,0,0, 0,1,0,1,0,0,1,0,1,0,0,0, rep(0,12))

AU1 <- c(0,200,200,0,100,0,200,100,100,200,0,100,
         100,0,200,100,100,200,200,200,0,0,0,100,
         rep(0,12))
Yes1 <- c(1,1,0,1,0,1,0,0,1,0,0,1,
          1,1,1,1,1,0,1,0,0,0,0,0,
          rep(0,12))

DB2 <- c(600,400,600,400,800,400,800,400,600,600,800,800,
         600,600,400,800,600,400,800,600,400,800,400,800,
         rep(0,12))
X302 <- c(1,0,0,0,0,1,0,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,rep(0,12))

X202 <- c(0,1,0,0,0,0,0,0,0,1,1,1,
         1,0,0,1,0,0,0,1,1,0,0,0,
         rep(0,12))

X242 <- c(0,0,1,0,1,1,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,rep(0,12))

AU2 <- c(100,200,100,0,100,200,0,100,200,0,200,0,
         100,0,100,200,0,200,0,200,100,200,0,100,
         rep(0,12))
Yes2 <- c(0,1,0,0,1,1,1,0,1,0,1,0,
          0,1,0,0,1,1,1,0,1,0,1,0,
          rep(0,12))


dm_land_use_dummy$DB1[dm_land_use_dummy$DB1 !=0] <-  DB1
dm_land_use_dummy$X301<- ifelse(dm_land_use_dummy$DBVar1 ==30, 1, 0)
dm_land_use_dummy$X201 <- ifelse(dm_land_use_dummy$ND1 ==20, 1, 0)
dm_land_use_dummy$X241 <- ifelse(dm_land_use_dummy$ND1 ==24, 1, 0)

dm_land_use_dummy$AU1[seq(1,108,3)] <- AU1
dm_land_use_dummy$No1[seq(1,108,3)] <- ifelse(Yes1==1, 0,1)
dm_land_use_dummy$DB2[dm_land_use_dummy$DB2 !=0] <- DB2
dm_land_use_dummy$X302<- ifelse(dm_land_use_dummy$DBVar2 ==30, 1, 0)
dm_land_use_dummy$X202 <- ifelse(dm_land_use_dummy$ND2 ==20, 1, 0)
dm_land_use_dummy$X242 <- ifelse(dm_land_use_dummy$ND2 ==24, 1, 0)
dm_land_use_dummy$AU2[seq(2,108,3)] <- AU2
dm_land_use_dummy$No2[seq(1,108,3)] <- ifelse(Yes2==1, 0,1)

dm_land_use_dummy <- select(dm_land_use_dummy, !starts_with(c("DBVar", "ND")))
saveRDS(dm_land_use_dummy, "data_field/design_matrix_dummy.rds")
        
library(readr)
library(dplyr)
dat_pretest <-  read_csv("data/data_pretest_final_comref.csv") %>% as.data.frame()
dat <- dat_pretest[, c("BLOCK", "q1", "q2", "q3", "q4", "q5", "q6", 
                               "q7", "q8", "q9", "q10", "q11", "q12")]
dat$ID <- seq(1,nrow(dat),1)
colnames(dat)[1] <- "BLOCK"

df <- make.dataset(
respondent.dataset = dat,
choice.indicators =
c("q1", "q2", "q3", "q4", "q5","q6", "q7", "q8", "q9", "q10", "q11", "q12"),
design.matrix = dm_land_use)

fm_land_use <- RES ~ ASC1 + No1 + DB1 + DBVar1 + ND1 + AU1 + # variables for Imported
  ASC2 + No2 + DB2 + DBVar2 + ND2 + AU2 + # variables for Domestic
  strata(STR) # stratification variable

out_land_use <- clogit(fm_land_use, data = df)
summary(out_land_use)

mwtp_land_use <- mwtp(
output = out_land_use,
monetary.variables = c("DB1", "DB2"),
nonmonetary.variables = list( c("ASC1","No1","AU1", "ND1", "DBVar1"), 
                              c("ASC2","No2", "AU2", "ND2", "DBVar2")),
confidence.level = 0.95,
method = "kr",
seed = 987)

### gamlss test
library(gamlss)

#df$RES <- ifelse(df$RES == 0, 1, 2)
# model<-gamlss(RES ~ ASC1 + No1 + DB1 + DBVar1 + ND1 + AU1 + # variables for Imported
#   ASC2 + No2 + DB2 + DBVar2 + ND2 + AU2 -1 ,
#               #sigma.fo=~NIR+RE+G+R+RE:G+NIR:G,
#               data=df,family=MN3, n.cyc=1500,c.crit=.01,trace=F)

### multinomial forest
library(orf)
library(ranger)
dm1 <- make.design.matrix(
choice.experiment.design = d_land_use,
optout = T, # include opt-out option
categorical.attributes = c("Greening"),
continuous.attributes = c("DB", "DBVar", "ND", "AU"),
unlabeled = T,
# common = common_alt
)

dm1$DB <- c( 800, 600, 0, 400, 400, 0, 800, 600, 0, 400, 400, 0, 400, 800, 0, 800, 
         400, 0, 600, 800, 0, 600,400, 0, 600, 600, 0, 400, 600, 0, 600, 800, 0,
         800,800, 0, 600,600, 0, 600,600, 0, 400,400, 0, 400,800, 0, 400,600, 0,
         600,400, 0, 800,800, 0, 800,600, 0, 800,400, 0, 400,800, 0, 800,400, 0, 
         600,800, 0,
         #Block3
         800,800,0,600,800,0,600,400,0,400,600,0,400,600,0,400,800,0,800,600,0,
         600,400,0,600,600,0,800,400,0,400,400,0,800,800,0)  

dm1$DBVar <- c( 30,30,0,30,15,0,15,15,0,15,15,0,30,15,0,15,30,0,15,15,0,15,30,0,15,
            30,0,30,30,0,30,30,0,30,15,0,15,15,0,30,15,0,30,15,0,30,30,0,15,30,
            0,30,15,0,15,30,0,15,30,0,15,15,0,15,15,0,30,30,0,30,30,0,
            #Block3
            15,30,0,15,30,0,30,30,0,15,15,0,30,30,0,15,30,0,30,15,0,15,15,0,30,
            15,0,30,30,0,15,30,0,30,15,0) 

dm1$ND <- c(20,16,0,20,20,0,16,24,0,16,16,0,24,24,0, 24,24,0, 20,16,0,24,16,0,20,24,
        0,24,20,0,16,20,0,16,20,0,20,20,0,24,24,0,20,24,0,24,20,0,16,16,0,16,16,
        0,24,16,0,16,20,0,24,20,0,20,16,0,20,24,0,16,24,0,
        #Block3
        24,24,0,20,20,0,16,24,0,16,24,0,24,16,0,20,24,0,20,20,0,24,20,0,24,16,0,
        20,16,0,16,20,0,16,16,0)

dm1$AU <- c(0,100,0,200,200,0,200,100,0,0,0,0,100,100,0,0,200,0,200,0,0,100,100,0,
        100,200,0,200,0,0,0,200,0,100,0,0,100,100,0,0,0,0,200,100,0,100,200,0,
        100,0,0,200,200,0,200,0,0,200,200,0,0,100,0,0,200,0,0,0,0,100,100,0,
        #Block3
        200,200,0,200,100,0,200,0,0,100,200,0,200,100,0,0,0,0,0,0,0,100,0,0,0,
        200,0,100,200,0,0,100,0,100,100,0)

Yes <- c(1,0,0,1,1,0,0,0,0,1,0,0,0,1,0,1,1,0,0,1,0,0,0,0,1,1,0,0,0,0,0,1,0,1,0,0,
         1,0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,
         1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0)

dm1$No <- ifelse(Yes==1, 0,1)

saveRDS(dm1, "data_field/design_matrix.rds")

# make dataset
df_mrf <- make.dataset(
respondent.dataset = dat,
choice.indicators =
c("q1", "q2", "q3", "q4", "q5","q6", "q7", "q8", "q9", "q10", "q11", "q12"),
design.matrix = dm1)

# choice dummies
df_mrf$choice1 <- ifelse( df$RES == T & df$ASC1==1, 1, 0)
df_mrf$choice2 <- ifelse( df$RES == T & df$ASC2==1, 1, 0)
df_mrf$choice3 <- ifelse( df$RES == T & df$ASC1==0 & df$ASC2==0, 1, 0)

df_mrf <- df_mrf %>% 
 mutate( group = case_when( choice1 == 1 ~ 1,
                            choice2 == 1 ~ 2,
                            choice3 == 1 ~ 3))

forest1 <- ranger(choice1~No+DB+DBVar+ND+AU, data = filter(df_mrf, ALT == 1))
forest2 <- ranger(choice2~No+DB+DBVar+ND+AU, data = filter(df_mrf, ALT == 2))
forest3 <- ranger(choice3~No+DB+DBVar+ND+AU, data = filter(df_mrf, ALT == 3))

pred1 <- forest1$predictions
pred2 <- forest2$predictions
pred3 <- 1-forest1$predictions-forest2$predictions
  
plot(density(forest1$predictions))
plot(density(forest2$predictions))
plot(density(1-forest1$predictions-forest2$predictions))

library(ggplot2);library(reshape2)
g <- ggplot()
g <- g + geom_density(aes(pred1, fill= "1 KUP"), alpha=0.5)
g <- g + geom_density(aes(pred2, fill= "2 Agroforst"), alpha=0.5)
g + geom_density(aes(pred3, fill= "3 Acker"), alpha=0.5)
