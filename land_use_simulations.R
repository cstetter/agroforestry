# Load required libraries
library(tidyverse)       # Data manipulation and visualization
library(sf)              # Simple Features for spatial data
library(extrafont)       # Additional fonts for ggplot
library(readr)           # Reading data
library(dplyr)           # Data manipulation
library(support.CEs)     # Support functions for conditional events
library(survival)        # Survival analysis
library(data.table)      # Efficient data manipulation
library(mlogit)          # Multinomial logit models
library(broom)           # Tidying model results
library(ggh4x)           # Extra geoms and stats for ggplot2


source("SimDroughtFunApp.R")  # Source a custom R script

# Load data
df_sim <- readRDS("data/df_sim.rds")  # Load simulated data

# Load a model result
# resCR25cor <- readRDS("models/res_cor_13_410.rds")  # Load a model result
resCR25cor <- readRDS("models/models_new/res_cor_13_410.rds")  # Load a model result

# Load more data
dat <- readRDS("data/test_app_dat.rds")             # Load test application data
dat_sim_multi <- readRDS("data/test_app_dat_multi.rds")  # Load multi-test application data

# Extract model information
X <- model.matrix(resCR25cor)  # Extract model matrix
bi <- as.data.frame(fitted(resCR25cor, type = "parameters")[,-1])  # Extract fitted parameters

# Rename columns of 'bi'
colnames(bi) <- paste0("b_", stringr::str_replace(c("SRC", "AC", names(coef(resCR25cor)[3:27])), "[:]", ""))

# Combine 'ID' with fitted parameters and rename 'ID'
bi1 <- as.data.frame(cbind(dat$ID, bi)) %>% dplyr::rename(ID = 1)

# Join 'dat' with fitted parameters using 'ID'
dat1 <- left_join(dat, bi1, by = "ID")

# Combine 'dat' and 'bi' side by side
dat <- cbind(dat, bi)

# Set the default theme for ggplot2 plots to a black and white theme with a base font size of 16
theme_set(theme_bw(base_size=16))



# Simulate worst-case drought scenarios for different years and shock durations
df_worst_case <- lapply(c(2003, 2018), function(y) {
 lapply(1:5, function(x) {
  do.call(rbind, lapply(c(unique(dat$SCH), "09"), function(i) {
   sim_drought(
    AC_DB = 400, AC_DBVar = 30, AC_Duruse = 24, AC_AES = 0, AC_NoGreen = 1,
    SRC_DB = 400, SRC_DBVar = 30, SRC_Duruse = 24, SRC_AES = 0, SRC_NoGreen = 1,
    SQ_DB = 400, SQ_DBVar = 15, SQ_Duruse = 3,
    RegBez = i,
    shock_dur = x, extreme_year = y
   )
  }))
 })
})

# Simulate mid-case drought scenarios for different years and shock durations
df_mid_case <- lapply(c(2003, 2018), function(y) {
 lapply(1:5, function(x) {
  do.call(rbind, lapply(c(unique(dat$SCH), "09"), function(i) {
   sim_drought(
    AC_DB = 600, AC_DBVar = 22.5, AC_Duruse = 20, AC_AES = 100, AC_NoGreen = 0,
    SRC_DB = 600, SRC_DBVar = 22.5, SRC_Duruse = 20, SRC_AES = 100, SRC_NoGreen = 0,
    SQ_DB = 400, SQ_DBVar = 15, SQ_Duruse = 3,
    RegBez = i,
    shock_dur = x, extreme_year = y
   )
  }))
 })
})

# Simulate best-case drought scenarios for different years and shock durations
df_best_case <- lapply(c(2003, 2018), function(y) {
 lapply(1:5, function(x) {
  do.call(rbind, lapply(c(unique(dat$SCH), "09"), function(i) {
   sim_drought(
    AC_DB = 800, AC_DBVar = 15, AC_Duruse = 16, AC_AES = 200, AC_NoGreen = 0,
    SRC_DB = 800, SRC_DBVar = 15, SRC_Duruse = 16, SRC_AES = 200, SRC_NoGreen = 0,
    SQ_DB = 400, SQ_DBVar = 15, SQ_Duruse = 3,
    RegBez = i,
    shock_dur = x, extreme_year = y
   )
  }))
 })
})

# Simulate worst-case drought scenarios with policy support for different years and shock durations
df_worst_case_policy_supp <- lapply(c(2003, 2018), function(y) {
 lapply(1:5, function(x) {
  do.call(rbind, lapply(c(unique(dat$SCH), "09"), function(i) {
   sim_drought(
    AC_DB = 400, AC_DBVar = 30, AC_Duruse = 24, AC_AES = 200, AC_NoGreen = 0,
    SRC_DB = 400, SRC_DBVar = 30, SRC_Duruse = 24, SRC_AES = 200, SRC_NoGreen = 0,
    SQ_DB = 400, SQ_DBVar = 15, SQ_Duruse = 3,
    RegBez = i,
    shock_dur = x, extreme_year = y
   )
  }))
 })
})

# Simulate worst-case drought scenarios with minimum duration for different years and shock durations
df_worst_case_min_nd <- lapply(c(2003, 2018), function(y) {
 lapply(1:5, function(x) {
  do.call(rbind, lapply(c(unique(dat$SCH), "09"), function(i) {
   sim_drought(
    AC_DB = 400, AC_DBVar = 30, AC_Duruse = 16, AC_AES = 0, AC_NoGreen = 1,
    SRC_DB = 400, SRC_DBVar = 30, SRC_Duruse = 16, SRC_AES = 0, SRC_NoGreen = 1,
    SQ_DB = 400, SQ_DBVar = 15, SQ_Duruse = 3,
    RegBez = i,
    shock_dur = x, extreme_year = y
   )
  }))
 })
})

# Assign names to the simulation dataframes
names(df_worst_case) <- names(df_mid_case) <- 
 names(df_best_case) <- names(df_worst_case_policy_supp) <- 
 names(df_worst_case_min_nd) <- c("y2003", "y2018")

# Customize scenario labels for each simulation dataframe
for (y in c("y2003", "y2018")) {
 for (i in 1:5) {
  df_worst_case[[y]][[i]]$scenario <- str_wrap("Regular-case scenario", 15)
  df_worst_case_policy_supp[[y]][[i]]$scenario <- str_wrap("Regular-case w/ policy supp.", 15)
  df_worst_case_min_nd[[y]][[i]]$scenario <- str_wrap("Regular-case w/ minimum dur.", 15)
  df_mid_case[[y]][[i]]$scenario <- str_wrap("Mid-case scenario", 15)
  df_best_case[[y]][[i]]$scenario <- str_wrap("Best-case scenario", 15)
 }
}

# Customize shock labels for the first shock duration
for (y in c("y2003", "y2018")) {
 df_worst_case[[y]][[1]]$shock <- df_worst_case_policy_supp[[y]][[1]]$shock <- 
  df_worst_case_min_nd[[y]][[1]]$shock <- df_mid_case[[y]][[1]]$shock <- 
  df_best_case[[y]][[1]]$shock <- "1 extreme\nyear"
}

# Customize shock labels for shock durations 2 to 5
for (y in c("y2003", "y2018")) {
 for (i in 2:5) {
  df_worst_case[[y]][[i]]$shock <- df_worst_case_policy_supp[[y]][[i]]$shock <- 
   df_worst_case_min_nd[[y]][[i]]$shock <- df_mid_case[[y]][[i]]$shock <- 
   df_best_case[[y]][[i]]$shock <- paste0(i, " consecutive\nextreme years")
 }
}

# Customize modeled year labels for all scenarios and shock durations
for (i in 1:5) {
 df_worst_case[["y2003"]][[i]]$modeled_year <- df_worst_case_policy_supp[["y2003"]][[i]]$modeled_year <- 
  df_worst_case_min_nd[["y2003"]][[i]]$modeled_year <- df_mid_case[["y2003"]][[i]]$modeled_year <- 
  df_best_case[["y2003"]][[i]]$modeled_year <- "2003-like extreme year"
 
 df_worst_case[["y2018"]][[i]]$modeled_year <- df_worst_case_policy_supp[["y2018"]][[i]]$modeled_year <- 
  df_worst_case_min_nd[["y2018"]][[i]]$modeled_year <- df_mid_case[["y2018"]][[i]]$modeled_year <- 
  df_best_case[["y2018"]][[i]]$modeled_year <- "2018-like extreme year"
}

# Create a list of simulation results for different scenarios and years
list_sim_res <- lapply(c("y2003", "y2018"), function(y) {
 lapply(1:5, function(i) {
  rbind(df_worst_case[[y]][[i]],
        df_worst_case_policy_supp[[y]][[i]],
        df_worst_case_min_nd[[y]][[i]],
        df_mid_case[[y]][[i]],
        df_best_case[[y]][[i]]) %>%
   mutate(RegBez = factor(RegBez,
                          levels = c("09", "091", "092", "093", 
                                     "094", "095", "096", "097"),
                          labels = str_wrap(c("Bavaria",
                                              "Upper Bavaria",
                                              "Lower Bavaria",
                                              "Upper Palatinate",
                                              "Upper Franconia",
                                              "Middle Franconia",
                                              "Lower Franconia",
                                              "Swabia"), 10)),
          name = ifelse(name == "p_shock", "Shocked", "Baseline"))
 })
})

# Filter and mutate data for the shock year 2018
df2018 <- bind_rows(list_sim_res, .id = "column_label") %>%
 filter(RegBez == "Bavaria",
        modeled_year == "2018-like extreme year") %>% 
 mutate(rect_width = as.numeric(column_label) - 5.5,
        scenario = forcats::fct_relevel(
         scenario,
         str_wrap("Regular-case scenario", 15),
         str_wrap("Regular-case w/ policy supp.", 15),
         str_wrap("Regular-case w/ minimum dur.", 15),
         str_wrap("Mid-case scenario", 15),
         str_wrap("Best-case scenario", 15)
        ))

# Filter and mutate data for the shock year 2003
df2003 <- bind_rows(list_sim_res, .id = "column_label") %>%
 filter(RegBez == "Bavaria",
        modeled_year == "2003-like extreme year") %>% 
 mutate(rect_width = as.numeric(column_label) - 0.5,
        scenario = forcats::fct_relevel(
         scenario,
         str_wrap("Regular-case scenario", 15),
         str_wrap("Regular-case w/ policy supp.", 15),
         str_wrap("Regular-case w/ minimum dur.", 15),
         str_wrap("Mid-case scenario", 15),
         str_wrap("Best-case scenario", 15)
        ))

# Create data frames for rectangle placement
rect_data2018 <- data.frame(
 scenario = rep(unique(df2018$scenario), length(unique(df2018$shock))),
 shock = rep(unique(df2018$shock), each = length(unique(df2018$scenario))),
 xmin = 0,
 xmax = rep(unique((df2018$rect_width)), each = length(unique(df2018$scenario))),
 ymin = -Inf,
 ymax = Inf
)

rect_data2003 <- data.frame(
 scenario = rep(unique(df2003$scenario), length(unique(df2003$shock))),
 shock = rep(unique(df2003$shock), each = length(unique(df2003$scenario))),
 xmin = 0,
 xmax = rep(unique((df2003$rect_width)), each = length(unique(df2003$scenario))),
 ymin = -Inf,
 ymax = Inf
)

#### Main simulation plots ####

# Create a list of main plot objects
list_gg_main <- lapply(list(df2018, df2003), function(i) {
 i %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(col = ALT, lty = name, alpha = name), size = 0.7) +
  facet_grid(scenario ~ shock) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 10, by = 1)) +
  theme(
   panel.grid.minor.x = element_blank(),
   panel.spacing.x = unit(0.9, "lines"),
   panel.spacing.y = unit(0.5, "lines"),
   legend.position = "bottom",
   legend.key.size = unit(2, "line")
  ) +
  labs(
   x="Year (after 1st occurrence of extreme weather event)",
   y="Average probability of cultivation",
   colour = "Land Use System",
   lty = "Scenario"
  ) +
  scale_linetype_manual(values = c("Baseline" = "dashed", 
                                   "Shocked" = "solid")) +
  scale_color_manual(
   values = c("darkorange1", "forestgreen", "dodgerblue1"),
   labels = c("Alley Cropping", "Short Rotation Coppice", "Crop Farming")
  ) +
  scale_alpha_manual(values = c("Baseline" = 0.66, 
                                "Shocked" = 1)) +
  guides(
   colour = guide_legend(title.position = "top", title.align = 0.5),
   linetype = guide_legend(title.position = "top"),
   alpha = "none"
  )
})

# Add rectangles to the main plot for the year 2018 and 2003
gg_shock_scenario2018 <- list_gg_main[[1]] +
 geom_rect(
  data = rect_data2018,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), inherit.aes = F,
  alpha = 0.2,
  fill = "firebrick"
 )

ggsave(filename = paste0("img/gg_shock_scenario2018.png"), 
       plot = gg_shock_scenario2018,
       width = 9, height = 10)

gg_shock_scenario2003 <- list_gg_main[[2]] +
 geom_rect(
  data = rect_data2003,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), inherit.aes = F,
  alpha = 0.2,
  fill = "firebrick"
 )


ggsave(filename = paste0("img/gg_shock_scenario2003.png"), 
       plot = gg_shock_scenario2003,
       width = 9, height = 10)


#### Appendix: heterogeneity #### 
list_gg_shock_scenarios <- 
 # bind_rows(list_sim_res, .id = "column_label")  %>%
 lapply(1:2, function(i)
  lapply(1:5, function(y) list_sim_res[[i]][[y]] %>% 
          filter(RegBez!="Bavaria") %>% 
          ggplot(aes(x=year, y=value)) +
          geom_line(aes(col=ALT, lty=name, alpha=name), size=0.9) +
          facet_grid(
           RegBez~forcats::fct_relevel(scenario,
                                       str_wrap("Regular-case scenario", 15),
                                       str_wrap("Regular-case w/ policy supp.", 15),
                                       str_wrap("Regular-case w/ minimum dur.",  15),
                                       str_wrap("Mid-case scenario", 15),
                                       str_wrap("Best-case scenario", 15))) +
          scale_x_continuous(expand = c(0, 0),
                             breaks = seq(0, 10, by = 1)) +
          annotate("rect", xmin=0, xmax=(y-.5), ymin=-Inf, ymax=Inf, alpha=0.2,
                   fill="firebrick") +
          theme(panel.grid.minor.x = element_blank(),
                panel.spacing.x = unit(0.9, "lines"),
                panel.spacing.y = unit(0.5, "lines"),
                legend.position = "bottom",
                legend.key.size = unit(2,"line")) +
          labs(x="Year (after occurrence of extreme weather event)",
               y="Average probability of cultivation",
               colour="Land Use System", 
               lty="Scenario") +
          scale_linetype_manual(values = c("Baseline" = "dashed", "Shocked" = "solid")) +
          scale_color_manual(values = c("darkorange1", "forestgreen", "dodgerblue1"), 
                             labels = c("Alley Cropping", "Short Rotation Coppice", "Crop Farming")) +
          scale_alpha_manual(values = c("Baseline" = 0.66, "Shocked" = 1))+
          guides(colour = guide_legend(title.position = "top", title.align=0.5),
                 linetype=guide_legend(title.position = "top"),
                 alpha = "none")
  ))

lapply(1:5, function(i)
 ggsave(filename = paste0("img/ggShock", i, "Scenarios_2018.png"), 
        plot = list_gg_shock_scenarios[[2]][[i]],
        width = 9, height = 12))

lapply(1:5, function(i)
 ggsave(filename = paste0("img/ggShock", i, "Scenarios_2003.png"), 
        plot = list_gg_shock_scenarios[[1]][[i]],
        width = 9, height = 12))


# #### test gganimate ####
# library(gganimate)
# p <-  gg_shock_scenario2003 +
#  transition_reveal(as.integer(year)) + 
#  view_follow(fixed_y = TRUE, fixed_x = TRUE)+
#  coord_cartesian(clip = 'off') 
# 
# a <- animate(p, width = 900, height = 900, renderer = av_renderer(), fps=5)
# anim_save("img/animation.mp4", a)
# 
# 
# 
# #### ANL Artikel ####
# for (y in c("y2003", "y2018")) {
#  df_worst_case[[y]][[1]]$shock <- df_worst_case_policy_supp[[y]][[1]]$shock <- 
#   df_worst_case_min_nd[[y]][[1]]$shock <- df_mid_case[[y]][[1]]$shock <- 
#   df_best_case[[y]][[1]]$shock <- "1 Extremjahr"
# }
# 
# for (y in c("y2003", "y2018")) {
#  for (i in 2:5) {
#   df_worst_case[[y]][[i]]$shock <- df_worst_case_policy_supp[[y]][[i]]$shock <- 
#    df_worst_case_min_nd[[y]][[i]]$shock <- df_mid_case[[y]][[i]]$shock <- 
#    df_best_case[[y]][[i]]$shock <- paste0(i, " aufeinanderfolgende\nExtremjahre")
#  }
# }
# 
# 
# dt_shock_scenarios <- 
#  lapply(c("y2003", "y2018"), function(y)
#   do.call(rbind, df_worst_case[[y]]) %>%
#    mutate(RegBez = factor(RegBez,
#                           levels = c( "09", "091", "092", "093", 
#                                       "094", "095", "096", "097"),
#                           labels = str_wrap(c("Bayern",
#                                               "Upper Bavaria",
#                                               "Lower Bavaria",
#                                               "Upper Palatinate",
#                                               "Upper Franconia",
#                                               "Middle Franconia",
#                                               "Lower Franconia",
#                                               "Swabia"), 10)),
#           name=ifelse(name=="p_shock", "Shocked", "Baseline")) %>%
#    filter(RegBez=="Bayern")
#  )
# 
# 
# dt_shock_scenarios[[1]]$SimYear <- "2003-Wetterereignis"
# dt_shock_scenarios[[2]]$SimYear <- "2018-Wetterereignis"
# 
# dt <- do.call(rbind, dt_shock_scenarios)
# 
# dt$name <- with(dt, ifelse(name=="Shocked", "Nach Wetterschock",
#                            name))
# 
# 
# dat_rect <- data.frame(
#  shock = rep(unique(dt$shock)[c(1:5)],2),
#  scenario = rep(unique(dt$SimYear), each=5),
#  xmin = 0,
#  xmax = rep( c(0.5, 1.5, 2.5, 3.5, 4.5), 2),
#  ymin = 0,
#  ymax = Inf)
# 
# ggplot(dt, aes(x=year, y=value)) +
#  geom_line(aes(col=ALT, lty=name, alpha=name), size=0.9) +
#  facet_grid(SimYear~shock) +
#  scale_x_continuous(expand = c(0, 0),
#                     breaks = seq(0, 10, by = 1)) +
#  geom_rect(
#   data    = dat_rect,
#   mapping = aes(xmin = xmin,
#                 xmax = xmax,
#                 ymin = ymin,
#                 ymax = ymax), alpha = 0.1, fill="firebrick", inherit.aes = FALSE)+
#  theme(panel.grid.minor.x = element_blank(),
#        panel.spacing.x = unit(0.9, "lines"),
#        panel.spacing.y = unit(0.5, "lines"),
#        legend.position = "bottom",
#        legend.key.size = unit(2,"line")) +
#  labs(x="Jahr (nach Eintreten von Extremwettereignis)",
#       y="Durchschnittliche \nAnbauwahrscheinlichkeit",
#       colour="Landnutzungssystem", 
#       lty="Szenario") +
#  scale_linetype_manual(values = c("Baseline" = "dashed", "Nach Wetterschock" = "solid")) +
#  scale_color_manual(values = c("darkorange1", "dodgerblue1", "forestgreen"), 
#                     labels = c("Alley Cropping", "Kurzumtriebsplantage", "Ackerbau")) +
#  scale_alpha_manual(values = c("Baseline" = 0.66, "Nach Wetterschock" = 1))+
#  guides(colour = guide_legend(title.position = "top", title.align=0.5),
#         linetype=guide_legend(title.position = "top"),
#         alpha = "none")
# 
