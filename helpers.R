res_cor_13_410 <- readRDS("models/models_new/res_cor_13_410.rds")

dfml2 <- dfml
old <- na.omit(predict(res_cor_13_410, newdata = dfml))

AvgMargEff <- function(land_use, attribute, delta) {
 if (land_use=="AC") {
  lu <- 2
 } else if (land_use=="SRC") {
  lu=1
 } else if (land_use=="SQ") {
  lu=3
 }
 dfml2[[attribute]] <- ifelse(dfml$idx$alt == lu,
                              dfml[[attribute]] + delta,
                              dfml[[attribute]])
 new <- na.omit(predict(res_cor_13_410, newdata = dfml2))
 colMeans((new-old)/new*100)
}


AvgMargEffDummy <- function(land_use) {
 dfml2 <- dfml
 dfml3 <- dfml
 if (land_use=="AC") {
  lu <- 2
 } else if (land_use=="SRC") {
  lu=1
 } else if (land_use=="SQ") {
  lu=3
 }
 # Modify dfml2 and dfml3 based on the value of intercept_ac
 dfml2[["No"]] <- ifelse(dfml$idx$alt == lu, 0, dfml[["No"]])
 dfml3[["No"]] <- ifelse(dfml$idx$alt == lu, 1, dfml[["No"]])
 # Calculate predictions and return the column means difference
 diff_col_means <- colMeans(na.omit(predict(res_cor_13_410, newdata = dfml2)) -
                             na.omit(predict(res_cor_13_410, newdata = dfml3)))*100
 return(diff_col_means)
}



me <-rbind(AvgMargEff(land_use = "SRC", attribute = "DB", delta = 10),
           AvgMargEff(land_use = "SRC", attribute = "DBVar", delta = -1),
           AvgMargEff(land_use = "SRC", attribute = "ND", delta = -1),
           AvgMargEff(land_use = "SRC", attribute = "AU", delta = 10),
           AvgMargEffDummy(land_use = "SRC"),
           AvgMargEff(land_use = "AC", attribute = "DB", delta = 10),
           AvgMargEff(land_use = "AC", attribute = "DBVar", delta = -1),
           AvgMargEff(land_use = "AC", attribute = "ND", delta = -1),
           AvgMargEff(land_use = "AC", attribute = "AU", delta = 10),
           AvgMargEffDummy(land_use = "AC")
)


me <- as.data.frame(me)
colnames(me) <- c("eff_on_sq", "eff_on_src", "eff_on_ac")
me
me$change_in <- rep(c("Attribute change in\nshort rotation coppice",
                                  "Attribute change in\nalley-cropping"), each=5)


me$attribute <- factor(c("DB", "DBVar", "ND", "AU", "Green"),
                       labels = 
                        c("Contribution margin\n(\u0394 €10)", 
                          "Contribution margin\nvariability\n(\u0394 1%)", 
                          "Minimum useful\nlifetime\n(\u0394 1 year)", 
                          "Agri-environmental\npayment\n(\u0394 €10)",
                          "Ecological priority\narea\n(Yes vs. No)"))


ggME <- pivot_longer(me, 1:3) %>%
 ggplot() +
 geom_col(aes(x = factor(name, levels = c("eff_on_sq", "eff_on_src", "eff_on_ac")), y = value, fill = name), col="black", width = .66) +
 facet_grid(change_in ~ attribute, scales = "free_x") +
 geom_hline(yintercept = 0) +
 theme_bw(base_size = 16) +
 scale_fill_manual(values = c("eff_on_ac"="darkorange1", "eff_on_sq"="dodgerblue1", "eff_on_src"="forestgreen"),
                   labels = c("eff_on_ac" = "Alley cropping", "eff_on_sq" = "Status quo", "eff_on_src" = "Short rotation coppice")) +
 coord_flip() +
 theme(
  axis.text.y = element_blank(),     # Remove x-axis labels
  axis.title.y = element_blank(),     # Remove y-axis title
  axis.ticks = element_blank(),      # Remove axis ticks
  legend.position = "bottom", 
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
 ) +
 labs(fill = "") +
 ylab("Average percentage change w.r.t. land-use selection probability")


ggsave(filename = "img/ggME.png",ggME, 
       dpi = "print", width = 10, height = 5.75)
