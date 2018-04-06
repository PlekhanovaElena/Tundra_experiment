## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Function that plots differences between plots and blocks with confidence intervals
#
plot_diff <- function(dat, yname, varname) {
  sins <- summarySE(dat, measurevar=yname, groupvars=c("block","plot"), na.rm = T)
  sins2 <- summarySE(dat, measurevar=yname, groupvars=c("block"), na.rm = T)
  colnames(sins)[4] = "y"
  colnames(sins2)[3] = "y"
  sins2$type = mean(sins2$y[sins2$block %in% levels(sins2$block)[1:5]])
  sins2$type[sins2$block %in% levels(sins2$block)[6:10]] = mean(sins2$y[sins2$block %in% levels(sins2$block)[6:10]])
  
  pd <- position_dodge(0.1) # move them .05 to the left and right
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#FF3399", "#663300", "#993399")
  
  # Use 95% confidence interval instead of SEM
  gg1 = ggplot(sins, aes(x=plot, y=y, colour=block)) + 
    geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=.1, position=pd) +
    ylab(yname) +
    geom_point(position=pd) + theme_bw() + scale_colour_manual(values=cbPalette) +
    ggtitle(varname) + theme(plot.title = element_text(hjust = 0.5))
  
  gg2 = ggplot(sins2, aes(x=block, y=y, colour=block, , group = type)) + 
    geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=.1, position=pd) +
    ylab(yname) +
    geom_point(position=pd) + theme_bw() + scale_colour_manual(values=cbPalette) +
    geom_errorbar(aes(ymax=type, ymin=type), linetype="longdash", colour = "grey")
  
  gg_left <- cowplot::plot_grid(gg1 + rremove("legend"), gg2 + rremove("legend"), ncol=1, nrow=2)
  gg_res <- cowplot::plot_grid(gg_left, cowplot::get_legend(gg1), rel_widths = c(5, 1))
  return(gg_res)
  #ggarrange(gg1, gg2, ncol=1, nrow=2, common.legend = F, legend="right")
}