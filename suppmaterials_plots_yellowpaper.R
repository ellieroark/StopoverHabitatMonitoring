################################
## SUPPLEMENTARY PLOTS: ARUs vs pt cts
## Point Abbaye 2019
## 
## author: Ellie Roark and Willson Gaul
## created: 1 May 2020
## last modified: 19 January 2021
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
library(patchwork)


#### Species Richness plots-----------------------------------------------------
# plot of coefficients with 95% confidence intervals for max.rand.spdetmm, a
# GLMM for species detected over time depending on count type and weather 
# variables

# this result is in the main plots script as a table of coefficient values
ggplot(data = maxrand_df,
             aes(x = factor(variable,
                            levels = c("Count type (ARU- 10 min consecutive)",
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
                                       "Count type (ARU- 22 min random)*Day of year squared")),
                 y = coef_estimate)) +
        geom_point() +
        geom_linerange(aes(x = factor(variable,
                                      levels = c("Count type (ARU- 10 min consecutive)",
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
                                                 "Count type (ARU- 22 min random)*Day of year squared")),
                           ymin = l_bound,
                           ymax = h_bound)) +
        xlab("Predictor Variable") +
        ylab("Coefficient estimate") +
        theme_bw() +
        ylim(-1, 1) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))




#### End species richness plots-------------------------------------------------

### create BRT summary plots for all species------------------------------------
# create a list, where each element is a list of plots for a single species
brt_summary_plots_all_sp <- lapply(brt_params, function(x) {
  list(ptct_plot = NA, aru_plot = NA)})

for(i in 1:length(brt_params)) {
  ## Point count models
  ## scatterplot of number of this sp per day over time, with fitted 
  ## average model (BRT) as a line
  this_sp <- names(brt_params)[i]
  fits_brt <- tryCatch(readRDS(paste0("fits_", this_sp, "_brt.rds")), 
                       error = function(x) NA)
  
  # get standardized predictions for predictions to days in the test data fold 
  # from all 1000 models
  preds_brt <- tryCatch({bind_rows(lapply(fits_brt, FUN = function(x) {
    bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
  }))}, 
  error = function(x) NA)
  
  # get average prediction for each day from the 200 iterations of the 5-fold CV
  preds_brt <- tryCatch({group_by(preds_brt, day_of_yr) %>%
    summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
              se = std.error(predictions))}, 
    error = function(x) NA)
  p1 <- tryCatch({ggplot(data = preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
      geom_point(data = sum_species_dfs[[this_sp]], aes(x = day_of_yr, y = resp), 
                 colour = "dark grey") + 
      geom_line(size=1) + 
      xlab("") + 
      scale_x_continuous(breaks = c(91, 105, 121, 135), 
                         labels = c("April 1", "April 15", "May 1", "May 15")) +
      ylab(expression(A[p])) + 
      ggtitle(this_sp) + 
      theme_bw() +
      theme(axis.text.y = element_text(size = t_size-2), 
            axis.title.y = element_text(size= t_size +1),
            axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                       vjust = 1))}, 
      error = function(x) NA)
  brt_summary_plots_all_sp[[this_sp]]$ptct_plot <- p1
  rm(fits_brt, p1, preds_brt)
  
  ## A66r models
  fits_brt <- tryCatch({readRDS(paste0("fits_", this_sp, "_aru66r_brt.rds"))}, 
                       error = function(x) NA)
  
  # get standardized predictions for predictions to days in the test data fold 
  # from all 1000 models
  preds_brt <- tryCatch({bind_rows(lapply(fits_brt, FUN = function(x) {
    bind_rows(lapply(x, FUN = function(y) {y$standardized_preds}))
  }))}, 
  error = function(x) NA)
  
  # get average prediction for each day from the 200 iterations of the 5-fold CV
  preds_brt <- tryCatch({group_by(preds_brt, day_of_yr) %>%
    summarise(mean_pred = mean(predictions), sdev = sd(predictions), 
              se = std.error(predictions))}, 
    error = function(x) NA)
  p1 <- tryCatch({ggplot(data = preds_brt, aes(x = day_of_yr, y = mean_pred)) + 
    geom_point(data = sum_aru_dfs[[this_sp]], aes(x = day_of_yr, y = resp), 
               colour = "dark grey") + 
    geom_line(size=1) + 
    xlab("") + 
    scale_x_continuous(breaks = c(91, 105, 121, 135), 
                       labels = c("April 1", "April 15", "May 1", "May 15")) +
    ylab(expression(A[66*R])) + 
    ggtitle(this_sp) + 
    theme_bw() +
    theme(axis.text.y = element_text(size = t_size-2), 
          axis.title.y = element_text(size= t_size +1),
          axis.text.x = element_text(size = t_size-2, angle = 40, hjust = 1, 
                                     vjust = 1))}, 
    error = function(x) NA)
  brt_summary_plots_all_sp[[this_sp]]$aru_plot <- p1
  rm(fits_brt, p1, preds_brt)
}

# order species by lowest to highest correlations (of model predictions)
brt_summary_plots_all_sp_ordered <- list()
for(i in 1:nrow(cor_all_sp[cor_all_sp$type == "Predicted", ])) {
  this_sp <- cor_all_sp$species[cor_all_sp$type == "Predicted"][i]
  brt_summary_plots_all_sp_ordered[[i]] <- brt_summary_plots_all_sp[[this_sp]]
}
names(brt_summary_plots_all_sp_ordered) <- cor_all_sp$species[
  cor_all_sp$type == "Predicted"]

# for(i in 1:length(brt_summary_plots_all_sp_ordered)) {
#   try(print(brt_summary_plots_all_sp_ordered[[i]]$ptct_plot +
#               brt_summary_plots_all_sp_ordered[[i]]$aru_plot))
# }
### end BRT summary plots for all species
#### end BRT abundance summary plots--------------------------------------------

## BRT plots showing out of bag test error for example model iterations
## for WIWR and GCKI -----------------------------------------------------------
ex_brts_gcki <- readRDS("example_fitted_brt_gcki1.rds")

ntree_err_df_gcki <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_gcki, 1:length(ex_brts_gcki), SIMPLIFY = FALSE))

gp <- ggplot(data = ntree_err_df_gcki, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 2000) + 
  ggtitle(expression(A[p]))

rm(ex_brts_gcki)

## arugcki
ex_brts_arugcki <- readRDS("example_fitted_brt_arugcki.rds")

ntree_err_df_arugcki <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_arugcki, 1:length(ex_brts_arugcki), SIMPLIFY = FALSE))

g30c <- ggplot(data = ntree_err_df_arugcki, aes(x = ntrees, y = err, 
                                                color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*C]))

rm(ex_brts_arugcki)

## arugcki10r
ex_brts_arugcki10r <- readRDS("example_fitted_brt_arugcki10r.rds")

ntree_err_df_arugcki10r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_arugcki10r, 1:length(ex_brts_arugcki10r), SIMPLIFY = FALSE))

g30r <- ggplot(data = ntree_err_df_arugcki10r, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*R]))

rm(ex_brts_arugcki10r)

##aru22rgcki
ex_brts_arugcki22r <- readRDS("example_fitted_brt_arugcki22r.rds")

ntree_err_df_arugcki22r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_arugcki22r, 1:length(ex_brts_arugcki22r), SIMPLIFY = FALSE))

g66r <- ggplot(data = ntree_err_df_arugcki22r, aes(x = ntrees, y = err,
                                                   color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 2000)+ 
  ggtitle(expression(A[66*R]))

rm(ex_brts_arugcki22r)

##wiwr
ex_brts_wiwr <- readRDS("example_fitted_brt_wiwr1.rds")

ntree_err_df_wiwr <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_wiwr, 1:length(ex_brts_wiwr), SIMPLIFY = FALSE))

wp <- ggplot(data = ntree_err_df_wiwr, aes(x = ntrees, y = err, 
                                     color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[p]))

rm(ex_brts_wiwr)

##aruwiwr
ex_brts_aruwiwr <- readRDS("example_fitted_brt_aruwiwr.rds")

ntree_err_df_aruwiwr <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_aruwiwr, 1:length(ex_brts_aruwiwr), SIMPLIFY = FALSE))

w30c <- ggplot(data = ntree_err_df_aruwiwr, aes(x = ntrees, y = err, 
                                        color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*C]))

rm(ex_brts_aruwiwr)

##aruwiwr10r
ex_brts_aruwiwr10r <- readRDS("example_fitted_brt_aruwiwr10r.rds")

ntree_err_df_aruwiwr10r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_aruwiwr10r, 1:length(ex_brts_aruwiwr10r), SIMPLIFY = FALSE))

w30r <- ggplot(data = ntree_err_df_aruwiwr10r, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000)+ 
  ggtitle(expression(A[30*R]))

rm(ex_brts_aruwiwr10r)

##aruwiwr22r
ex_brts_aruwiwr22r <- readRDS("example_fitted_brt_aruwiwr22r.rds")

ntree_err_df_aruwiwr22r <- bind_rows(mapply(FUN = function(x, fold) {
  data.frame(ntrees = 1:length(x$mod$cv.error), 
             err = x$mod$cv.error, 
             fold = fold)
}, ex_brts_aruwiwr22r, 1:length(ex_brts_aruwiwr22r), SIMPLIFY = FALSE))

w66r <- ggplot(data = ntree_err_df_aruwiwr22r, aes(x = ntrees, y = err, 
                                                   color = factor(fold))) + 
  geom_line() + 
  ylab("Absolute error (test data)") + 
  xlab("Number of trees") + 
  scale_color_viridis_d(name = "CV fold") + 
  theme_bw() + 
  geom_vline(xintercept = 3000) + 
  ggtitle(expression(A[66*R]))

rm(ex_brts_aruwiwr22r)

brt_testerr_wiwr <- wp + w30c + w30r + w66r +
  plot_layout(ncol = 2, guides = 'collect') +
  plot_annotation(title = 'Winter Wren', 
                  tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 10))

brt_testerr_gcki <- gp + g30c + g30r + g66r + 
  plot_layout(ncol = 2, guides = 'collect') +
  plot_annotation(title = 'Golden-crowned Kinglet', 
                  tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 10))

## BRT plots showing out of bag test error for example model interations 
## for all species--------------------------------------------------------------
# point counts
for(i in 1:length(names(brt_params))) {
  this_sp <- names(brt_params)[i]
  ex_brts_thisSp <- tryCatch({
    readRDS(paste0("example_fitted_brt_", this_sp, ".rds"))}, 
    error = function(x) NA)
  ntree_err_df_thisSp <- tryCatch({bind_rows(mapply(FUN = function(x, fold) {
    data.frame(ntrees = 1:length(x$mod$cv.error), 
               err = x$mod$cv.error, 
               fold = fold)
  }, ex_brts_thisSp, 1:length(ex_brts_thisSp), SIMPLIFY = FALSE))}, 
  error = function (x) NA)
  gp <- tryCatch({
    ggplot(data = ntree_err_df_thisSp, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
      geom_line() + 
      ylab("Absolute error (test data)") + 
      xlab("Number of trees") + 
      scale_color_viridis_d(name = "CV fold") + 
      theme_bw() + 
      geom_vline(xintercept = brt_params[[this_sp]]$pt_ct[["nt"]]) + # nt we chose
      ggtitle(expression(A[p]), 
              subtitle = paste0(this_sp, " sr = ", 
                                ex_brts_thisSp[[1]]$mod$shrinkage))}, 
    error = function(x) NA)
  #try(print(gp))
  try(rm(ex_brts_thisSp, ntree_err_df_thisSp, gp))
}

# aru66r
for(i in 1:length(names(brt_params))) {
  this_sp <- names(brt_params)[i]
  ex_brts_thisSp <- tryCatch({
    readRDS(paste0("example_fitted_brt_aru66r_", this_sp, ".rds"))}, 
    error = function(x) NA)
  ntree_err_df_thisSp <- tryCatch({bind_rows(mapply(FUN = function(x, fold) {
    data.frame(ntrees = 1:length(x$mod$cv.error), 
               err = x$mod$cv.error, 
               fold = fold)
  }, ex_brts_thisSp, 1:length(ex_brts_thisSp), SIMPLIFY = FALSE))}, 
  error = function (x) NA)
  gp <- tryCatch({
    ggplot(data = ntree_err_df_thisSp, aes(x = ntrees, y = err, 
                                           color = factor(fold))) + 
      geom_line() + 
      ylab("Absolute error (test data)") + 
      xlab("Number of trees") + 
      scale_color_viridis_d(name = "CV fold") + 
      theme_bw() + 
      geom_vline(xintercept = brt_params[[this_sp]]$aru66r[["nt"]]) + # nt we chose
      ggtitle(expression(A[66*R]), 
              subtitle = paste0(this_sp, " sr = ", 
                                ex_brts_thisSp[[1]]$mod$shrinkage))}, 
    error = function(x) NA)
  #try(print(gp))
  try(rm(ex_brts_thisSp, ntree_err_df_thisSp, gp))
}
# end graphs for all species----------------------------------------------------

## species accumulation curves--------------------------------------------------
sp_ac_p1 <- ggplot() + 
  geom_line(data = sp_accum[sp_accum$April2_April14 == T, ],
            aes(x = minute, y = n_species_detected, 
                group = unique_replicate), position = "jitter", 
            alpha = 0.01) + 
  geom_smooth(data = sp_accum[sp_accum$April2_April14 == T, ], 
              aes(x = minute, y = n_species_detected),
              color = "dark orange") + 
  xlab("Minute")+
  ylab("Number of Species") + 
  ggtitle("April 2nd to April 14th") + 
  theme_bw()
  
sp_ac_p2 <- ggplot() + 
  geom_line(data = sp_accum[sp_accum$April15_April27 == T, ],
            aes(x = minute, y = n_species_detected, 
                group = unique_replicate), position = "jitter", 
            alpha = 0.01) + 
  geom_smooth(data = sp_accum[sp_accum$April15_April27 == T, ], 
              aes(x = minute, y = n_species_detected),
              color = "dark orange") + 
  xlab("Minute")+
  ylab("Number of Species") + 
  ggtitle("April 15th to April 27th") + 
  theme_bw()

sp_ac_p3 <- ggplot() + 
  geom_line(data = sp_accum[sp_accum$April28_May9 == T, ], 
            aes(x = minute, y = n_species_detected, 
                group = unique_replicate), position = "jitter", 
            alpha = 0.01) + 
  geom_smooth(data = sp_accum[sp_accum$April28_May9 == T, ], 
              aes(x = minute, y = n_species_detected),
              color = "dark orange") + 
  xlab("Minute")+
  ylab("Number of Species") + 
  ggtitle("April 28th to May 9th") + 
  theme_bw()

sp_ac_p4 <- ggplot() + 
  geom_line(data = sp_accum[sp_accum$May10_May22 == T, ], 
            aes(x = minute, y = n_species_detected, 
                group = unique_replicate), position = "jitter", 
            alpha = 0.01) + 
  geom_smooth(data = sp_accum[sp_accum$May10_May22 == T, ], 
              aes(x = minute, y = n_species_detected),
              color = "dark orange") + 
  xlab("Minute")+
  ylab("Number of Species") + 
  ggtitle("May 10th to May 22nd") + 
  theme_bw()

sp_ac <- sp_ac_p1 + sp_ac_p2 + sp_ac_p3 + sp_ac_p4 + 
  plot_layout(ncol = 2, guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 10))

## end species accumulation curves----------------------------------------------


## write out final plots for supp materials!------------------------------------
ggsave(brt_testerr_wiwr, filename = "./saved_objects/brt_testerr_wiwr.jpg", 
       width = 16, height = 15, 
       units = "cm", device = "jpeg")

ggsave(brt_testerr_gcki, filename = "./saved_objects/brt_testerr_gcki.jpg", 
       width = 16, height = 15, 
       units = "cm", device = "jpeg")

ggsave(sp_ac, filename = "./saved_objects/sp_accumulation_curves.jpg", 
       width = 16, height = 15, 
       units = "cm", device = "jpeg")

pdf("./saved_objects/brt_summary_allsp.pdf")
for(i in 1:length(brt_summary_plots_all_sp_ordered)) {
  try(print(brt_summary_plots_all_sp_ordered[[i]]$ptct_plot + 
              brt_summary_plots_all_sp_ordered[[i]]$aru_plot))
}
dev.off()

## end write out plots to file--------------------------------------------------
