################################
## PLOTS: ARUs vs pt cts
## Point Abbaye 2019
## 
## author: Ellie Roark
## created: 27 Nov 2019
## last modified: 1 May 2020
## 
## inputs: *see StopoverHabitatMonitoring.R for scripts which must be run
##          before this script can be sourced
##
##         
## outputs: *a number of plots for use in publications from the Pt Abbaye 2019
##            ARU vs Pt ct project.
##            
## TODO: * 
################################

library(wgutil)
library(tidyverse)
library(Hmisc)
library(lme4)
library(plotrix)
library(patchwork)

t_size <- 12
#setwd("/home/emer/Dropbox/Ellie Roark/R/PointAbbaye/")

### clean up environment
# rm(arugcki, arugcki.day.gam, arugcki.day.gam2, aruwiwr, aruwiwr.day.gam, 
#    aruwiwr.day.gam2)
##TODO add more to this list once all plots are created and I can remove 
##extraneous objects  

### BOOSTED REGRESSION TREE SUMMARY PLOT-------------------------------------

## scatterplot of number of GCKI per day (point counts) over time, with fitted 
## average model (BRT) as a line
fits_gcki_brt <- readRDS("fits_gcki_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_ptct_preds_brt <- bind_rows(lapply(fits_gcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
gcki_ptct_preds_brt <- group_by(gcki_ptct_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))
g4p1 <- ggplot(data = gcki_ptct_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_point(data = sum_gcki, aes(x = day_of_yr, y = resp), colour = "orange") + 
  geom_line() + 
  #ggtitle("Golden-crowned Kinglet\nPoint counts (10 consecutive min)") +
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  ylab(expression(A[p])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2), 
        axis.title.y = element_text(size= t_size +1),
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_gcki_brt)


## scatterplot of number of GCKI per day (ARU consecutive 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_arugcki_brt <- readRDS("fits_arugcki_brt.rds")

gcki_aru10c_preds_brt <- bind_rows(lapply(fits_arugcki_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
gcki_aru10c_preds_brt <- group_by(gcki_aru10c_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p2 <- ggplot(data = gcki_aru10c_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_point(data = sum_arugcki, aes(x = day_of_yr, y = resp), 
             colour = "orange") + 
  geom_line() + 
  #ggtitle("Golden-crowned Kinglet\nARU- 10 consecutive min") +
  xlab("") +
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0, 0.26), 
                     breaks = c(0.00, 0.05, 0.1, 0.15, 0.2, 0.25)) +
  ylab(expression(A[30*c])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2), 
        axis.title.y = element_text(size= t_size +1),
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_arugcki_brt)

 
## scatterplot of number of GCKI per day (ARU random 10 min) over time, 
## with fitted average model (BRT) as a line
fits_arugcki10r_brt <- readRDS("fits_arugcki10r_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_aru10r_preds_brt <- bind_rows(lapply(fits_arugcki10r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
gcki_aru10r_preds_brt <- group_by(gcki_aru10r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p3 <- ggplot(data = gcki_aru10r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_point(data = sum_arugcki10r, aes(x = day_of_yr, y = resp), 
             colour = "orange") + 
  geom_line() + 
  #ggtitle("Golden-crowned Kinglet\nARU- 10 random min") +
  xlab("") +
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0, 0.26), 
                     breaks = c(0.00, 0.05, 0.1, 0.15, 0.2, 0.25)) +
  #ylab("Abundance index") + 
  ylab(expression(A[30*r])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2), 
        axis.title.y = element_text(size= t_size +1),
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_arugcki10r_brt)

## scatterplot of number of GCKI per day (ARU random 22 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_arugcki22r_brt <- readRDS("fits_arugcki22r_brt.rds")

# get standardized predictions for predictions to test data from all 1000 models
gcki_aru22r_preds_brt <- bind_rows(lapply(fits_arugcki22r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
gcki_aru22r_preds_brt <- group_by(gcki_aru22r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p4 <- ggplot(data = gcki_aru22r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_point(data = sum_arugcki22r, aes(x = day_of_yr, y = resp), 
             colour = "orange") + 
  geom_line() + 
  #ggtitle("Golden-crowned Kinglet\nARU- 22 random min") +
  xlab("") +
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0, 0.26), 
                     breaks = c(0.00, 0.05, 0.1, 0.15, 0.2, 0.25)) +
  #ylab("Abundance index") + 
  ylab(expression(A[66*r])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2), 
        axis.title.y = element_text(size= t_size +1),
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))
rm(fits_arugcki22r_brt)



## scatterplot of number of WIWR per day (Point Counts 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_wiwr_brt <- readRDS("fits_wiwr_brt.rds")
wiwr_ptct_preds_brt <- bind_rows(lapply(fits_wiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
wiwr_ptct_preds_brt <- group_by(wiwr_ptct_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p5 <- ggplot(data = wiwr_ptct_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_line() + 
  geom_point(data = sum_wiwr, aes(x = day_of_yr, y = resp), 
             colour = "blue") + 
  # ggtitle("Winter Wren\nPoint counts (10 min); BRT") + 
  #ggtitle("Point Counts\n10 consecutive minutes\n(a)") + 
  xlab("") +
  scale_x_continuous(breaks = c(91, 105, 121, 135),
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  #ylab("Winter Wren\nAbundance index") + 
  ylab(expression(A[p])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1), 
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_wiwr_brt)


## scatterplot of number of WIWR per day (ARU consecutive 10 min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_aruwiwr_brt <- readRDS("fits_aruwiwr_brt.rds")

wiwr_aru10c_preds_brt <- bind_rows(lapply(fits_aruwiwr_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
wiwr_aru10c_preds_brt <- group_by(wiwr_aru10c_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p6 <- ggplot(data = wiwr_aru10c_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_point(data = sum_aruwiwr, aes(x = day_of_yr, y = resp), 
             colour = "blue") + 
  geom_line() + 
  #ggtitle("Winter Wren\nARU - consecutive 10 min; BRT") + 
  #ggtitle("ARU Counts\n10 consecutive minutes\n(b)") +
  #ylab("Abundance index") + 
  ylab(expression(A[30*c])) + 
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) + 
  scale_y_continuous(limits = c(0.0, 0.4), 
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2), 
        axis.title.y = element_text(size = t_size+1),
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_aruwiwr_brt)

## scatterplot of number of WIWR per day (ARU 10 rand min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_aruwiwr10r_brt <- readRDS("fits_aruwiwr10r_brt.rds")

wiwr_aru10r_preds_brt <- bind_rows(lapply(fits_aruwiwr10r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
wiwr_aru10r_preds_brt <- group_by(wiwr_aru10r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p7 <- ggplot(data = wiwr_aru10r_preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
  # geom_ribbon(aes(ymin = mean_pred - se,
  #                 ymax = mean_pred + se)) +
  geom_point(data = sum_aruwiwr10r, aes(x = day_of_yr, y = resp), 
             colour = "blue") + 
  geom_line() + 
  #ggtitle("Winter Wren\nARU (10 random min); BRT") + 
  #ggtitle("ARU Counts\n10 random minutes") +
  xlab("") +
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) + 
  scale_y_continuous(limits = c(0.0, 0.4), 
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4)) +
  #ylab("Abundance index") + 
  ylab(expression(A[30*r])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1),
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_aruwiwr10r_brt)

## scatterplot of number of WIWR per day (ARU 22 rand min) over time, 
## with fitted average model (BRT) as a line
# get standardized predictions for predictions to test data from all 1000 models
fits_aruwiwr22r_brt <- readRDS("fits_aruwiwr22r_brt.rds")

wiwr_aru22r_preds_brt <- bind_rows(lapply(fits_aruwiwr22r_brt, FUN = function(x) {
  bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
}))

# get average prediction for each day from the 200 iterations of the 5-fold CV
wiwr_aru22r_preds_brt <- group_by(wiwr_aru22r_preds_brt, day_of_yr) %>%
  summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
            se = std.error(predictions))

g4p8 <- ggplot(data = wiwr_aru22r_preds_brt, aes(x = day_of_yr, y = mean_pred)) +
 # geom_ribbon(aes(ymin = mean_pred - se,
 #                ymax = mean_pred + se)) +
  geom_point(data = sum_aruwiwr22r, aes(x = day_of_yr, y = resp), 
             colour = "blue") + 
  geom_line() + 
  #ggtitle("ARU Counts\n22 random minutes\n(d)") +
  #ggtitle("Winter Wren\nARU (22 random min); BRT") + 
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135),
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0.0, 0.4), 
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4)) +
  #ylab("Abundance index") + 
  ylab(expression(A[66*r])) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1), 
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

rm(fits_aruwiwr22r_brt)


## abundance data and BRT models for 2 species for all 4 survey methods
brt_summary <- g4p5 + g4p6 + g4p7 + g4p8 + g4p1 + g4p2 + g4p3 + g4p4 + 
  plot_layout(ncol = 4) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 10))


## experiment: just lines (no observed dots), and both species on the same plot.
## first need to combine all pred dataframes into one 
## (making copies of the gcki_xxx_preds_brt dfs so I don't mess up code down 
## script)
tgcki_ptct_preds_brt <- gcki_ptct_preds_brt
tgcki_aru10c_preds_brt <- gcki_aru10c_preds_brt
tgcki_aru10r_preds_brt <- gcki_aru10r_preds_brt
tgcki_aru22r_preds_brt <- gcki_aru22r_preds_brt
tgcki_ptct_preds_brt$count_type <- "ptct"
tgcki_aru10c_preds_brt$count_type <- "aru10c"
tgcki_aru10r_preds_brt$count_type <- "aru10r"
tgcki_aru22r_preds_brt$count_type <- "aru22r"
gcki_preds_brt <- bind_rows(tgcki_ptct_preds_brt, tgcki_aru10c_preds_brt, 
                            tgcki_aru10r_preds_brt, tgcki_aru22r_preds_brt)
gcki_preds_brt$species <- "GCKI"

twiwr_ptct_preds_brt <- wiwr_ptct_preds_brt
twiwr_aru10c_preds_brt <- wiwr_aru10c_preds_brt
twiwr_aru10r_preds_brt <- wiwr_aru10r_preds_brt 
twiwr_aru22r_preds_brt <- wiwr_aru22r_preds_brt
twiwr_ptct_preds_brt$count_type <- "ptct"
twiwr_aru10c_preds_brt$count_type <- "aru10c"
twiwr_aru10r_preds_brt$count_type <- "aru10r"
twiwr_aru22r_preds_brt$count_type <- "aru22r"
wiwr_preds_brt <- bind_rows(twiwr_ptct_preds_brt, twiwr_aru10c_preds_brt, 
                            twiwr_aru10r_preds_brt, twiwr_aru22r_preds_brt)
wiwr_preds_brt$species <- "WIWR"

preds_brt <- bind_rows(wiwr_preds_brt, gcki_preds_brt)

sp2p1 <- ggplot(data = preds_brt[which(preds_brt$count_type == "ptct"), ], 
              aes(x = day_of_yr, y = mean_pred, colour = species)) +
  geom_line(aes(x = day_of_yr, y = mean_pred, colour = species)) + 
  ggtitle("(a)") +
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135),
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  ylab(expression(A[p])) +   
  scale_colour_viridis_d(name = "Species", labels = c("Golden-crowned\nKinglet", 
                                                      "Winter Wren"),
                         option = "magma", 
                         begin = 0.1, end = 0.75) +
  theme_bw() +
  theme(plot.title = element_text(size = t_size-2),
        axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1), 
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

sp2p2 <- ggplot(data = preds_brt[which(preds_brt$count_type == "aru10c"), ], 
                aes(x = day_of_yr, y = mean_pred, colour = species)) +
  geom_line(aes(x = day_of_yr, y = mean_pred, colour = species)) + 
  ggtitle("(b)") +
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135),
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0.0, 0.15), 
                     breaks = c(0.0, 0.05, 0.1, 0.15)) +
  ylab(expression(A[30*C])) + 
  scale_colour_viridis_d(name = "Species", labels = c("Golden-crowned\nKinglet", 
                                                      "Winter Wren"),
                         option = "magma", 
                         begin = 0.1, end = 0.75) +
  theme_bw() +
  theme(plot.title = element_text(size = t_size-2),
        axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1), 
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

sp2p3 <- ggplot(data = preds_brt[which(preds_brt$count_type == "aru10r"), ], 
                aes(x = day_of_yr, y = mean_pred, colour = species)) +
  geom_line(aes(x = day_of_yr, y = mean_pred, colour = species)) + 
  ggtitle("(c)") +
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135),
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0.0, 0.15), 
                     breaks = c(0.0, 0.05, 0.1, 0.15)) +
  ylab(expression(A[30*R])) + 
  scale_colour_viridis_d(name = "Species", labels = c("Golden-crowned\nKinglet", 
                                                      "Winter Wren"),
                         option = "magma", 
                         begin = 0.1, end = 0.75) +
  theme_bw() +
  theme(plot.title = element_text(size = t_size-2),
        axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1), 
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

sp2p4 <- ggplot(data = preds_brt[which(preds_brt$count_type == "aru22r"), ], 
                aes(x = day_of_yr, y = mean_pred, colour = species)) +
  geom_line(aes(x = day_of_yr, y = mean_pred, colour = species)) + 
  ggtitle("(d)") +
  xlab("") + 
  scale_x_continuous(breaks = c(91, 105, 121, 135),
                     labels = c("April 1", "April 15", "May 1", "May 15")) +
  scale_y_continuous(limits = c(0.0, 0.15), 
                     breaks = c(0.0, 0.05, 0.1, 0.15)) +
  ylab(expression(A[66*R])) + 
  scale_colour_viridis_d(name = "Species", labels = c("Golden-crowned\nKinglet", 
                                                      "Winter Wren"),
                         option = "magma", 
                         begin = 0.1, end = 0.75) +
  theme_bw() +
  theme(plot.title = element_text(size = t_size-2),
        axis.text.y = element_text(size = t_size-2),
        axis.title.y = element_text(size = t_size+1), 
        axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                   vjust = 1))

brt_summary_sp2 <- sp2p1 + sp2p2 + sp2p3 + sp2p4 + 
  plot_layout(ncol = 4, guides = 'collect')

### END BRT SUMMARY PLOT--------------------------------------------------------


### ABUNDANCE CORRELATION PLOTS-------------------------------------------------

## correlation plots for predicted and observed abundance metrics
# make single df for WIWR w/predicted and observed values from all survey methods
abund_wiwr_pred <- list(point_count = wiwr_ptct_preds_brt, 
                        aru10c = wiwr_aru10c_preds_brt, 
                        aru10r = wiwr_aru10r_preds_brt, 
                        aru22r = wiwr_aru22r_preds_brt)
abund_wiwr_pred <- mapply(FUN = function(x, m) {x$method <- m; x}, 
                          abund_wiwr_pred, names(abund_wiwr_pred), 
                          SIMPLIFY = F, USE.NAMES = T)
abund_wiwr_pred <- bind_rows(abund_wiwr_pred)
abund_wiwr_pred <- subset(abund_wiwr_pred, subset = TRUE,  select=-c(sdev, se))
abund_wiwr_pred_wide <- pivot_wider(abund_wiwr_pred, names_from = method, 
                               values_from = mean_pred)
abund_wiwr_pred_wide$type <- "predicted"

# make df of observed values
abund_wiwr_obs <- list(point_count = sum_wiwr, 
                       aru10c = sum_aruwiwr, 
                       aru10r = sum_aruwiwr10r, 
                       aru22r = sum_aruwiwr22r)
abund_wiwr_obs <- mapply(FUN = function(x, m) {x$method <- m; x}, 
                          abund_wiwr_obs, names(abund_wiwr_obs), 
                          SIMPLIFY = F, USE.NAMES = T)
abund_wiwr_obs <- bind_rows(abund_wiwr_obs)
abund_wiwr_obs_wide <- dplyr::select(abund_wiwr_obs, day_of_yr, resp, method) %>% 
  pivot_wider(names_from = method, values_from = resp)
abund_wiwr_obs_wide$type <- "observed"

# combine predicted and observed abundance values
abund_wiwr_wide <- bind_rows(abund_wiwr_pred_wide, abund_wiwr_obs_wide)

## make correlation plots for WIWR
## point count + aru10c -- observed
c1wo <- ggplot(data = abund_wiwr_obs_wide, aes(x = aru10c, y = point_count)) + 
  geom_point() + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*C])) + 
  #scale_x_continuous(breaks = c(0.1, 0.3)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru10r
c2wo <- ggplot(data = abund_wiwr_obs_wide, aes(x = aru10r, y = point_count)) + 
  geom_point() + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*R])) + 
  #scale_x_continuous(breaks = c(0.1, 0.3)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru22r
c3wo <- ggplot(data = abund_wiwr_obs_wide, aes(x = aru22r, y = point_count)) + 
  geom_point() + 
  ylab(expression(A[p])) + 
  xlab(expression(A[66*R])) + 
  #scale_x_continuous(breaks = c(0.1, 0.3)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru10r
c4wo <- ggplot(data = abund_wiwr_obs_wide, aes(x = aru10r, y = aru10c)) + 
  geom_point() + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[30*R])) + 
  #scale_x_continuous(breaks = c(0.1, 0.3)) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru22r
c5wo <- ggplot(data = abund_wiwr_obs_wide, aes(x = aru22r, y = aru10c,)) + 
  geom_point() + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[66*R])) + 
  #scale_x_continuous(breaks = c(0.1, 0.3)) + 
  theme_bw() +
  theme(text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

obscorplot_wiwr <- c1wo + c2wo + c3wo + guide_area() + c4wo + c5wo + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = t_size))

## point count + aru10c -- predicted
c1wp <- ggplot(data = abund_wiwr_pred_wide, aes(x = aru10c, y = point_count)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*C])) + 
  scale_x_continuous(breaks = round(seq(min(abund_wiwr_pred_wide$aru10c), 
                                        max(abund_wiwr_pred_wide$aru10c), 
                                        by = 0.005), 3)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru10r
c2wp <- ggplot(data = abund_wiwr_pred_wide, aes(x = aru10r, y = point_count)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*R])) + 
  scale_x_continuous(breaks = round(seq(min(abund_wiwr_pred_wide$aru10r), 
                                        max(abund_wiwr_pred_wide$aru10r), 
                                        by = 0.025), 2)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru22r
c3wp <- ggplot(data = abund_wiwr_pred_wide, aes(x = aru22r, y = point_count)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[p])) + 
  xlab(expression(A[66*R])) + 
  scale_x_continuous(breaks = round(seq(min(abund_wiwr_pred_wide$aru22r), 
                                        max(abund_wiwr_pred_wide$aru22r), 
                                        by = 0.05), 2)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru10r
c4wp <- ggplot(data = abund_wiwr_pred_wide, aes(x = aru10r, y = aru10c)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[30*R])) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru22r
c5wp <- ggplot(data = abund_wiwr_pred_wide, aes(x = aru22r, y = aru10c,)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[66*R])) + 
  #scale_x_continuous(breaks = c(0.04, 0.12)) + 
  theme_bw() +
  theme(text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

predcorplot_wiwr <- c1wp + c2wp + c3wp + guide_area() + c4wp + c5wp + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = t_size))



# make single df for GCKI w/predicted and observed values from all survey methods
abund_gcki_pred <- list(point_count = gcki_ptct_preds_brt, 
                        aru10c = gcki_aru10c_preds_brt, 
                        aru10r = gcki_aru10r_preds_brt, 
                        aru22r = gcki_aru22r_preds_brt)
abund_gcki_pred <- mapply(FUN = function(x, m) {x$method <- m; x}, 
                          abund_gcki_pred, names(abund_gcki_pred), 
                          SIMPLIFY = F, USE.NAMES = T)
abund_gcki_pred <- bind_rows(abund_gcki_pred)
abund_gcki_pred <- subset(abund_gcki_pred, subset = TRUE,  select=-c(sdev, se))
abund_gcki_pred_wide <- pivot_wider(abund_gcki_pred, names_from = method, 
                                    values_from = mean_pred)
abund_gcki_pred_wide$type <- "predicted"

# make df of observed values
abund_gcki_obs <- list(point_count = sum_gcki, 
                       aru10c = sum_arugcki, 
                       aru10r = sum_arugcki10r, 
                       aru22r = sum_arugcki22r)
abund_gcki_obs <- mapply(FUN = function(x, m) {x$method <- m; x}, 
                         abund_gcki_obs, names(abund_gcki_obs), 
                         SIMPLIFY = F, USE.NAMES = T)
abund_gcki_obs <- bind_rows(abund_gcki_obs)
abund_gcki_obs_wide <- dplyr::select(abund_gcki_obs, day_of_yr, resp, method) %>% 
  pivot_wider(names_from = method, values_from = resp)
abund_gcki_obs_wide$type <- "observed"

# combine predicted and observed abundance values
abund_gcki_wide <- bind_rows(abund_gcki_pred_wide, abund_gcki_obs_wide)

# create correlation plots for abundance indices to combine into multiplot

# ## point count + aru10c
# c1g <- ggplot(data = abund_gcki_wide, aes(x = aru10c, y = point_count)) + 
#   geom_point(shape = 1, aes(colour = type)) + 
#   # scale_shape(solid = FALSE) + 
#   ylab(expression(A[p])) + 
#   xlab(expression(A[10*c])) + 
#   scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.75) + 
#   theme_bw() + 
#   theme(legend.position="none", text = element_text(size = 14))
# 
# ## point count + aru10r
# c2g <- ggplot(data = abund_gcki_wide, aes(x = aru10r, y = point_count)) + 
#   geom_point(shape = 1, aes(colour = type)) + 
#   ylab(expression(A[p])) + 
#   xlab(expression(A[10*r])) + 
#   scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.75) + 
#   theme_bw() + 
#   theme(legend.position="none", text = element_text(size = 14))
# 
# ## point count + aru22r
# c3g <- ggplot(data = abund_gcki_wide, aes(x = aru22r, y = point_count)) + 
#   geom_point(shape = 1, aes(colour = type)) + 
#   ylab(expression(A[p])) + 
#   xlab(expression(A[22*r])) + 
#   scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.75) + 
#   theme_bw() + 
#   theme(legend.position="none", text = element_text(size = 14))
# 
# ## aru10c + aru10r
# c4g <- ggplot(data = abund_gcki_wide, aes(x = aru10r, y = aru10c)) + 
#   geom_point(shape = 1, aes(colour = type)) + 
#   ylab(expression(A[10*c])) + 
#   xlab(expression(A[10*r])) + 
#   scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.75) + 
#   theme_bw() +
#   theme(legend.position="none", text = element_text(size = 14))
# 
# ## aru10c + aru22r
# c5g <- ggplot(data = abund_gcki_wide, aes(x = aru22r, y = aru10c,)) + 
#   geom_point(shape = 1, aes(colour = type)) + 
#   ylab(expression(A[10*c])) + 
#   xlab(expression(A[22*r])) + 
#   scale_colour_viridis_d(name = "Data type", option = "inferno", 
#                          begin = 0.2, end = 0.75) + 
#   theme_bw() +
#   theme(text = element_text(size = 14))
# 
# corplot_gcki <- c1g + c2g + c3g + guide_area() + c4g + c5g + 
#   plot_layout(guides = 'collect') + 
#   plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
#   theme(plot.tag = element_text(size = t_size))

## make GCKI correlation plots as above, but separately for observed and 
## predicted values

## point count + aru10c -- observed
c1go <- ggplot(data = abund_gcki_obs_wide, aes(x = aru10c, y = point_count)) + 
  geom_point() + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*C])) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru10r
c2go <- ggplot(data = abund_gcki_obs_wide, aes(x = aru10r, y = point_count)) + 
  geom_point() + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*R])) + 
  #scale_x_continuous(breaks = c(0.05, 0.15)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru22r
c3go <- ggplot(data = abund_gcki_obs_wide, aes(x = aru22r, y = point_count)) + 
  geom_point() + 
  ylab(expression(A[p])) + 
  xlab(expression(A[66*r])) + 
  #scale_x_continuous(breaks = c(0.03, 0.09)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru10r
c4go <- ggplot(data = abund_gcki_obs_wide, aes(x = aru10r, y = aru10c)) + 
  geom_point() + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[30*R])) + 
  #scale_x_continuous(breaks = c(0.05, 0.15)) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru22r
c5go <- ggplot(data = abund_gcki_obs_wide, aes(x = aru22r, y = aru10c,)) + 
  geom_point() + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[66*R])) + 
  #scale_x_continuous(breaks = c(0.03, 0.09)) + 
  theme_bw() +
  theme(text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

obscorplot_gcki <- c1go + c2go + c3go + guide_area() + c4go + c5go + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = t_size))

## point count + aru10c -- predicted
c1gp <- ggplot(data = abund_gcki_pred_wide, aes(x = aru10c, y = point_count)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*C])) + 
  scale_x_continuous(breaks = round(seq(min(abund_gcki_pred_wide$aru10c),
                                        max(abund_gcki_pred_wide$aru10c),
                                        by = 0.0005), 3)) +
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru10r
c2gp <- ggplot(data = abund_gcki_pred_wide, aes(x = aru10r, y = point_count)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[p])) + 
  xlab(expression(A[30*R])) + 
  scale_x_continuous(breaks = round(seq(min(abund_gcki_pred_wide$aru10r), 
                                        max(abund_gcki_pred_wide$aru10r), 
                                        by = 0.0025), 3)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## point count + aru22r
c3gp <- ggplot(data = abund_gcki_pred_wide, aes(x = aru22r, y = point_count)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[p])) + 
  xlab(expression(A[66*R])) + 
  scale_x_continuous(breaks = round(seq(min(abund_gcki_pred_wide$aru22r), 
                                        max(abund_gcki_pred_wide$aru22r), 
                                        by = 0.005), 3)) + 
  theme_bw() + 
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru10r
c4gp <- ggplot(data = abund_gcki_pred_wide, aes(x = aru10r, y = aru10c)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[30*R])) + 
  scale_x_continuous(breaks = round(seq(min(abund_gcki_pred_wide$aru10r), 
                                  max(abund_gcki_pred_wide$aru10r), 
                                  by = 0.0025), 3)) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

## aru10c + aru22r
c5gp <- ggplot(data = abund_gcki_pred_wide, aes(x = aru22r, y = aru10c,)) + 
  geom_point(shape = 17) + 
  ylab(expression(A[30*C])) + 
  xlab(expression(A[66*R])) + 
  scale_x_continuous(breaks = round(seq(min(abund_gcki_pred_wide$aru22r), 
                                        max(abund_gcki_pred_wide$aru22r), 
                                        by = 0.005), 3)) + 
  theme_bw() +
  theme(text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

predcorplot_gcki <- c1gp + c2gp + c3gp + guide_area() + c4gp + c5gp + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = t_size))


#### END ABUNDANCE CORRELATION PLOT---------------------------------------------

### ABUNDANCE CORRELATION COEFFICIENT TABLE-------------------------------------
gcki_obs_cor <-rcorr(as.matrix(abund_gcki_obs_wide[,2:5]), type = "spearman")
gcki_obs_cor <- data.frame(gcki_obs_cor$r)
gcki_obs_cor_l <- gcki_obs_cor %>% 
  rownames_to_column(var = "count_type1") %>%
  pivot_longer(-count_type1, names_to = "count_type2", values_to = "gcki_obs_corr")
# gcki_obs_cor_l$count_type <- paste(gcki_obs_cor_l$count_type1, 
#                                    gcki_obs_cor_l$count_type2, 
#                         sep = " and ")
# gcki_obs_cor_l$count_type1 <- NULL
# gcki_obs_cor_l$count_type2 <- NULL

gcki_pred_cor <-rcorr(as.matrix(abund_gcki_pred_wide[,2:5]), type = "spearman")
gcki_pred_cor <- data.frame(gcki_pred_cor$r)
gcki_pred_cor_l <- gcki_pred_cor %>% 
  rownames_to_column(var = "count_type1") %>%
  pivot_longer(-count_type1, names_to = "count_type2", values_to = "gcki_pred_corr")
# gcki_pred_cor_l$count_type <- paste(gcki_pred_cor_l$count_type1, 
#                                    gcki_pred_cor_l$count_type2, 
#                                    sep = " and ")
# gcki_pred_cor_l$count_type1 <- NULL
# gcki_pred_cor_l$count_type2 <- NULL


wiwr_pred_cor <-rcorr(as.matrix(abund_wiwr_pred_wide[,2:5]), type = "spearman")
wiwr_pred_cor <- data.frame(wiwr_pred_cor$r)
wiwr_pred_cor_l <- wiwr_pred_cor %>% 
  rownames_to_column(var = "count_type1") %>%
  pivot_longer(-count_type1, names_to = "count_type2", values_to = "wiwr_pred_corr")
# wiwr_pred_cor_l$count_type <- paste(wiwr_pred_cor_l$count_type1, 
#                                     wiwr_pred_cor_l$count_type2, 
#                                     sep = " and ")
# wiwr_pred_cor_l$count_type1 <- NULL
# wiwr_pred_cor_l$count_type2 <- NULL


wiwr_obs_cor <-rcorr(as.matrix(abund_wiwr_obs_wide[,2:5]), type = "spearman")
wiwr_obs_cor <- data.frame(wiwr_obs_cor$r)
wiwr_obs_cor_l <- wiwr_obs_cor %>% 
  rownames_to_column(var = "count_type1") %>%
  pivot_longer(-count_type1, names_to = "count_type2", values_to = "wiwr_obs_corr")
# wiwr_obs_cor_l$count_type <- paste(wiwr_obs_cor_l$count_type1, 
#                                     wiwr_obs_cor_l$count_type2, 
#                                     sep = " and ")
# wiwr_obs_cor_l$count_type1 <- NULL
# wiwr_obs_cor_l$count_type2 <- NULL


abund_corr <- left_join(gcki_obs_cor_l, gcki_pred_cor_l)
abund_corr_w <- left_join(wiwr_obs_cor_l, wiwr_pred_cor_l)
abund_corr <- left_join(abund_corr, abund_corr_w)

# drop identity combos
abund_corr <- abund_corr[abund_corr$count_type1 != abund_corr$count_type2, ]
# drop duplicates (that have the count types in reverse order)
abund_corr <- abund_corr[abund_corr$count_type2 != "point_count", ]
abund_corr <- abund_corr[!grepl("aru22r|aru10r", abund_corr$count_type1), ]

### END ABUNDANCE CORRELATION COEFFICIENT TABLE---------------------------------

#### SPECIES RICHNESS GLMM COEFFICIENT TABLE------------------------------------
## table of coefficients for GLMM (max.rand.spdetmm-- includes all count types)
## # make a data frame with all variable names for the model
maxrand_df <- data.frame(matrix(nrow = 19, ncol = 4))
colnames(maxrand_df) <- c("variable", "coef_estimate", "l_bound", "h_bound")
maxrand_df$variable <- c("Count type (ARU- 10 min consecutive)", 
                    "Count type (ARU- 10 min random)", 
                    "Count type (ARU- 22 min random)", "Wind (2)", "Wind (3+)",
                    "Rain (Wet)", "Noise (1)", "Noise (>2)", "Day of year", 
                    "Day of year squared",
                    "Count type (ARU- 10 min consecutive)*Rain (Wet)", 
                    "Count type (ARU- 10 min random)*Rain (Wet)",
                    "Count type (ARU- 22 min random)*Rain (Wet)",
                    "Count type (ARU- 10 min consecutive)*Day of Year",
                    "Count type (ARU- 10 min random)*Day of Year",
                    "Count type (ARU- 22 min random)*Day of Year",
                    "Count type (ARU- 10 min consecutive)*Day of year squared",
                    "Count type (ARU- 10 min random)*Day of year squared",
                    "Count type (ARU- 22 min random)*Day of year squared")

# get coefficient point estimate and lower and upper CI bounds for each
# predictor variable
for(i in 1:nrow(maxrand_df)) {
  # get coefficient point estimate
  maxrand_df$coef_estimate[i] <- summary(max.rand.spdetmm)$coefficients[1 + i]
  maxrand_df$pvalue[i] <- summary(max.rand.spdetmm)$coefficients[1+i, "Pr(>|z|)"]
  maxrand_df$l_bound[i] <- CI.maxrand[2 + i, "2.5 %"]
  maxrand_df$h_bound[i] <- CI.maxrand[2 + i, "97.5 %"]
}
### END SPECIES RICHNESS GLMM COEFFICIENT TABLE---------------------------------

### SPECIES RICHNESS OVER TIME BY COUNT TYPE PLOT-------------------------------
##species detected over time, stratified by count type, with plotted GLMM
## prediction line, from max.rand.spdetmm model! formula: 
## sp_detected ~ count_type + wind + rain + noise + day_of_yr_s +  
## rain:count_type + day_of_yr_s:count_type 
##  (1 | day_of_yr_s)
#first, create new df with response and predictor variable columns
mean_doy <- mean(spdet_4ct$day_of_yr)
sd_doy_c <- sd(spdet_4ct$day_of_yr_c)
aru10c.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                       max(spdet_4ct$day_of_yr), by = 1))
aru10c.pred$day_of_yr_c <- aru10c.pred$day_of_yr-mean_doy
aru10c.pred$day_of_yr_s <- aru10c.pred$day_of_yr_c/sd_doy_c
#ptct.pred <- data.frame(day_of_yr_s = spdet_all$day_of_yr_s)
aru10c.pred$day_sq_s <- aru10c.pred$day_of_yr_s^2
aru10c.pred$count_type <- as.factor("aru")
aru10c.pred$wind <- as.factor("0-1")
aru10c.pred$rain <- as.factor("Dry")
aru10c.pred$noise <- as.factor("0")
aru10c.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru10c.pred$count_type <- factor(aru10c.pred$count_type, 
                              levels = c("point", "aru", "aru_10r", "aru_22r"), 
                              labels = c("point", "aru", "aru_10r", "aru_22r"))
aru10c.pred$wind <- factor(aru10c.pred$wind, 
                        levels = c("0-1", "2", "3+"),
                        labels = c("0-1", "2", "3+"))
aru10c.pred$rain <- factor(aru10c.pred$rain, 
                        levels = c("Dry", "wet"),
                        labels = c("Dry", "wet"))
aru10c.pred$noise <- factor(aru10c.pred$noise, 
                         levels = c("0", "1", ">2"), 
                         labels = c("0", "1", ">2"))
aru10c.pred$point_id <- factor(aru10c.pred$point_id, 
                            levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                       "p8", "p9", "p10", "p11", "p12", 
                                       "p13","p14", "p15", "p17", "p18", 
                                       "FB1", "FB2"),
                            labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                       "p8", "p9", "p10", "p11", "p12", 
                                       "p13","p14", "p15", "p17", "p18", 
                                       "FB1", "FB2"))



#next, feed that new data into the predict function for lmer package
#make a new col with predictions
aru10c.pred$preds <- predict(max.rand.spdetmm, newdata = aru10c.pred, 
                             type = "response", re.form = NA)


## now do the same thing with pt cts
# ptct.pred <- data.frame(day_of_yr_s = seq(-1.86, 1.61, by = 0.01))
ptct.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                        max(spdet_4ct$day_of_yr), by = 1))
ptct.pred$day_of_yr_c <- ptct.pred$day_of_yr-mean_doy
ptct.pred$day_of_yr_s <- ptct.pred$day_of_yr_c/sd_doy_c

#ptct.pred <- data.frame(day_of_yr_s = spdet_all$day_of_yr_s)
ptct.pred$day_sq_s <- ptct.pred$day_of_yr_s^2
ptct.pred$count_type <- as.factor("point")
ptct.pred$wind <- as.factor("0-1")
ptct.pred$rain <- as.factor("Dry")
ptct.pred$noise <- as.factor("0")
ptct.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
ptct.pred$count_type <- factor(ptct.pred$count_type, 
                               levels = c("point", "aru", "aru_10r", "aru_22r"), 
                               labels = c("point", "aru", "aru_10r", "aru_22r"))
ptct.pred$wind <- factor(ptct.pred$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
ptct.pred$rain <- factor(ptct.pred$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
ptct.pred$noise <- factor(ptct.pred$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
ptct.pred$point_id <- factor(ptct.pred$point_id, 
                             levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"),
                             labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"))

#now make a new col with predictions
ptct.pred$preds <- predict(max.rand.spdetmm, type = "response",
                           newdata = ptct.pred, re.form = NA)

## now do the same thing with aru10rand
aru10r.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                        max(spdet_4ct$day_of_yr), by = 1))
aru10r.pred$day_of_yr_c <- aru10r.pred$day_of_yr-mean_doy
aru10r.pred$day_of_yr_s <- aru10r.pred$day_of_yr_c/sd_doy_c
aru10r.pred$day_sq_s <- aru10r.pred$day_of_yr_s^2
aru10r.pred$count_type <- as.factor("aru_10r")
aru10r.pred$wind <- as.factor("0-1")
aru10r.pred$rain <- as.factor("Dry")
aru10r.pred$noise <- as.factor("0")
aru10r.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru10r.pred$count_type <- factor(aru10r.pred$count_type, 
                               levels = c("point", "aru", "aru_10r", "aru_20r"), 
                               labels = c("point", "aru", "aru_10r", "aru_20r"))
aru10r.pred$wind <- factor(aru10r.pred$wind, 
                         levels = c("0-1", "2", "3+"),
                         labels = c("0-1", "2", "3+"))
aru10r.pred$rain <- factor(aru10r.pred$rain, 
                         levels = c("Dry", "wet"),
                         labels = c("Dry", "wet"))
aru10r.pred$noise <- factor(aru10r.pred$noise, 
                          levels = c("0", "1", ">2"), 
                          labels = c("0", "1", ">2"))
aru10r.pred$point_id <- factor(aru10r.pred$point_id, 
                             levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"),
                             labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                        "p8", "p9", "p10", "p11", "p12", 
                                        "p13","p14", "p15", "p17", "p18", 
                                        "FB1", "FB2"))

#now make a new col with predictions
aru10r.pred$preds <- predict(max.rand.spdetmm, type = "response",
                           newdata = aru10r.pred, re.form = NA)

## now do the same thing with aru22rand
aru22r.pred <- data.frame(day_of_yr = seq(min(spdet_4ct$day_of_yr), 
                                          max(spdet_4ct$day_of_yr), by = 1))
aru22r.pred$day_of_yr_c <- aru22r.pred$day_of_yr-mean_doy
aru22r.pred$day_of_yr_s <- aru22r.pred$day_of_yr_c/sd_doy_c

aru22r.pred$day_sq_s <- aru22r.pred$day_of_yr_s^2
aru22r.pred$count_type <- as.factor("aru_22r")
aru22r.pred$wind <- as.factor("0-1")
aru22r.pred$rain <- as.factor("Dry")
aru22r.pred$noise <- as.factor("0")
aru22r.pred$point_id <- as.factor("FB1")
#coerce factor variables to contain the same number of levels as the original 
aru22r.pred$count_type <- factor(aru22r.pred$count_type, 
                                 levels = c("point", "aru", "aru_10r", "aru_22r"), 
                                 labels = c("point", "aru", "aru_10r", "aru_22r"))
aru22r.pred$wind <- factor(aru22r.pred$wind, 
                           levels = c("0-1", "2", "3+"),
                           labels = c("0-1", "2", "3+"))
aru22r.pred$rain <- factor(aru22r.pred$rain, 
                           levels = c("Dry", "wet"),
                           labels = c("Dry", "wet"))
aru22r.pred$noise <- factor(aru22r.pred$noise, 
                            levels = c("0", "1", ">2"), 
                            labels = c("0", "1", ">2"))
aru22r.pred$point_id <- factor(aru22r.pred$point_id, 
                               levels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                          "p8", "p9", "p10", "p11", "p12", 
                                          "p13","p14", "p15", "p17", "p18", 
                                          "FB1", "FB2"),
                               labels = c("p1", "p2", "p3", "p4", "p5", "p6",
                                          "p8", "p9", "p10", "p11", "p12", 
                                          "p13","p14", "p15", "p17", "p18", 
                                          "FB1", "FB2"))

#now make a new col with predictions
aru22r.pred$preds <- predict(max.rand.spdetmm, type = "response",
                             newdata = aru22r.pred, re.form = NA)

#bind ptct.pred to aru10c.pred to aru10r.pred to aru22r.pred
pred.4ct <- bind_rows(ptct.pred, aru10c.pred, aru10r.pred, aru22r.pred)

spdet_time <- ggplot(pred.4ct, 
                     aes(x=day_of_yr, y=preds, 
                         color= factor(count_type, 
                                       levels = c("point", "aru", 
                                                  "aru_10r", "aru_22r"), 
                                       labels = c(expression(S[p]),
                                                  expression(S[10*C]),
                                                  expression(S[10*R]),
                                                  expression(S[22*R])))), 
                         group = factor(count_type, 
                                        levels = c("point", "aru", 
                                                   "aru_10r", "aru_22r"), 
                                        labels = c(expression(S[p]),
                                                   expression(S[10*C]),
                                                   expression(S[10*R]),
                                                   expression(S[22*R])))) + 
  # geom_point(shape=1) +
  geom_line(size = t_size/12) + 
  geom_point(data = spdet_4ct,
             aes(x = day_of_yr, y = sp_detected, 
                 color = factor(count_type, 
                                levels = c("point", "aru", 
                                           "aru_10r", "aru_22r"), 
                                labels = c(expression(S[p]),
                                           expression(S[10*C]),
                                           expression(S[10*R]),
                                           expression(S[22*R])))), 
             size = t_size/12) +
  scale_colour_viridis_d(name = "Species\nRichness\nIndex", option = "magma", 
                         begin = 0, end = 0.75, 
                         labels = c(expression(S[p]),
                                    expression(S[10*C]),
                                    expression(S[10*R]),
                                    expression(S[22*R]))) + 
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) + 
  scale_x_continuous(breaks = c(91, 105, 121, 135), 
                     labels = c("April 1", "April 15", "May 1", "May 15")) + 
  ylab("Number of Species") +
  xlab("") + 
  theme_bw() + 
  theme(text = element_text(size = 12))
  
spdet_time
#### SPECIES RICHNESS OVER TIME BY COUNT TYPE-----------------------------------


### write out plots as jpgs ---------------------------------------------------
ggsave(spdet_time, filename = "./saved_objects/spdet_time.jpg", 
       width = 15, height = 10, 
       units = "cm", device = "jpeg")

ggsave(obscorplot_gcki, filename = "./saved_objects/obscorplot_gcki.jpg", 
       width = 15, height = 10, 
       units = "cm", device = "jpeg")

ggsave(predcorplot_gcki, filename = "./saved_objects/predcorplot_gcki.jpg", 
       width = 15, height = 10, 
       units = "cm", device = "jpeg")

ggsave(obscorplot_wiwr, filename = "./saved_objects/obscorplot_wiwr.jpg", 
       width = 15, height = 10, 
       units = "cm", device = "jpeg")

ggsave(predcorplot_wiwr, filename = "./saved_objects/predcorplot_wiwr.jpg", 
       width = 15, height = 10, 
       units = "cm", device = "jpeg")

ggsave(brt_summary, filename = "./saved_objects/brt_summary.jpg", 
       width = 20, height = 15, 
       units = "cm", device = "jpeg")

ggsave(brt_summary_sp2, filename = "./saved_objects/brt_summary_sp2.jpg", 
       width = 20, height = 8, 
       units = "cm", device = "jpeg")

### write out tables as .csvs---------------------------------------------------
# write out table of mixed model results for species richness model with four
# count types. 
maxrand_df[,-1] <- round(maxrand_df[,-1], digits = 4)
write_csv(maxrand_df, path = "./saved_objects/mixedmodel_results_speciesrichness.csv")

abund_corr[,3:6] <- round(abund_corr[,3:6], digits = 3)
write_csv(abund_corr, path = "./saved_objects/abund_index_corr_coefficients.csv")

### print numbers needed for manuscript-----------------------------------------
## mean RMSE for all BRTs
rmse_gcki_brt <- readRDS("rmse_gcki_brt.rds")
meanrmse_gcki_brt <- mean(rmse_gcki_brt)
rmse_arugcki_brt <- readRDS("rmse_arugcki_brt.rds")
meanrmse_arugcki_brt <- mean(rmse_arugcki_brt)
rmse_arugcki10r_brt <- readRDS("rmse_arugcki10r_brt.rds")
meanrmse_arugcki10r_brt <- mean(rmse_arugcki10r_brt)
rmse_arugcki22r_brt <- readRDS("rmse_arugcki22r_brt.rds")
meanrmse_arugcki22r_brt <- mean(rmse_arugcki22r_brt)

rmse_wiwr_brt <- readRDS("rmse_wiwr_brt.rds")
meanrmse_wiwr_brt <- mean(rmse_wiwr_brt)
rmse_aruwiwr_brt <- readRDS("rmse_aruwiwr_brt.rds")
meanrmse_aruwiwr_brt <- mean(rmse_aruwiwr_brt)
rmse_aruwiwr10r_brt <- readRDS("rmse_aruwiwr10r_brt.rds")
meanrmse_aruwiwr10r_brt <- mean(rmse_aruwiwr10r_brt)
rmse_aruwiwr22r_brt <- readRDS("rmse_aruwiwr22r_brt.rds")
meanrmse_aruwiwr22r_brt <- mean(rmse_aruwiwr22r_brt)

rmse_gcki_gam <- readRDS("rmse_gcki_gam.rds")
meanrmse_gcki_gam <- mean(rmse_gcki_gam)
rmse_arugcki_gam <- readRDS("rmse_arugcki_gam.rds")
meanrmse_arugcki_gam <- mean(rmse_arugcki_gam)
rmse_wiwr_gam <- readRDS("rmse_wiwr_gam.rds")
meanrmse_wiwr_gam <- mean(rmse_wiwr_gam)
rmse_aruwiwr_gam <- readRDS("rmse_aruwiwr_gam.rds")
meanrmse_aruwiwr_gam <- mean(rmse_aruwiwr_gam)
