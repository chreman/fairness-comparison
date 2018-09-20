remove(list = ls())

library(stringr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(robust)
library(purrr)
library(reshape2)
library(hclust)
library(plyr)

setwd("/home/chris/projects/fairness-comparison/")

# Use these for more than 10 algorithms
colors_20 = c(
  "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", "#d62728", "#ff9896",
  "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#7f7f7f", "#c7c7c7",
  "#bcbd22", "#dbdb8d", "#17becf", "#9edae5")

# Use these for 10 or fewer algorithms
colors_10 = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

catscale10  = scale_colour_manual(values=colors_10)
catscale10_2 = scale_fill_manual(values=colors_10)

catscale20  = scale_colour_manual(values=colors_20)
catscale20_2 = scale_fill_manual(values=colors_20)

algos_to_plot = c(
  "ZafarFairness", "Calders", "SVM",
  "Feldman-SVM", "DecisionTree",
  "Feldman-DecisionTree", "Kamishima")

create_sensitivity_summary <- function(ss, x_var, y_var) {
  ss <- ss[,c('algorithm', 'metricName', 'sensitiveType', 'metric')]
  ssg <- ss %>% group_by(algorithm, metricName, sensitiveType)
  res <- data.frame(dcast(ssg, algorithm ~ metricName, mean)["algorithm"])
  res["x_mean"] <- dcast(ssg, algorithm ~ metricName, mean)[x_var]
  res["x_sd"] <- dcast(ssg, algorithm ~ metricName, sd)[x_var]
  res["y_mean"] <- dcast(ssg, algorithm ~ metricName, mean)[y_var]
  res["y_sd"] <- dcast(ssg, algorithm ~ metricName, sd)[y_var]
  return(res)
}

create_sensitivity_summary_old = function(df, x_var, y_var) {
  quo_x_var = enquo(x_var)
  quo_y_var = enquo(y_var)
  #x_mean_name = paste0("mean_", quo_name(quo_x_var))
  #y_mean_name = paste0("mean_", quo_name(quo_y_var))
  #x_sd_name = paste0("sd_", quo_name(quo_x_var))
  #y_sd_name = paste0("sd_", quo_name(quo_y_var))
  
  sensitivity_summary =
    df %>%
    group_by(algorithm) %>%
    summarise(x_sd=sd(!!quo_x_var), x_mean=mean(!!quo_x_var),
              y_sd=sd(!!quo_y_var), y_mean=mean(!!quo_y_var))
  sensitivity_summary
}

plot_sensitivity = function(df, var1, var2) {
  x_var = var1
  y_var = var2
  
  sensitivity_summary = create_sensitivity_summary(df, x_var, y_var)
  basic_plot = basic_sensitivity_plot(sensitivity_summary, x_var, y_var)
  plot_rects_sensitivity_summary(basic_plot)
}

plot_sensitivity_old = function(df, var1, var2) {
  x_var = as.name(var1)
  y_var = as.name(var2)
  
  sensitivity_summary = create_sensitivity_summary_old(df, !!x_var, !!y_var)
  basic_plot = basic_sensitivity_plot_old(sensitivity_summary, !!x_var, !!y_var)
  plot_rects_sensitivity_summary(basic_plot)
}

basic_sensitivity_plot = function(sensitivity_summary, x_var, y_var) {
  ggplot(sensitivity_summary, aes(x=x_mean, y=y_mean, colour=algorithm)) + catscale10 + catscale10_2 +
    xlab(x_var) + ylab(y_var)
}

basic_sensitivity_plot_old = function(sensitivity_summary, x_var, y_var) {
  ggplot(sensitivity_summary, aes(x=x_mean, y=y_mean, colour=algorithm)) + catscale10 + catscale10_2 +
    xlab(quo_name(enquo(x_var))) + ylab(quo_name(enquo(y_var))) + xlim(-0, 1) + ylim(-0, 1)
}


plot_rects_sensitivity_summary = function(basic_plot) {
  aesthetics1 = aes(xmin = x_mean - 0.5 * x_sd,   
                    xmax = x_mean + 0.5 * x_sd,
                    ymin = y_mean - 0.5 * y_sd,
                    ymax = y_mean + 0.5 * y_sd,
                    fill=algorithm)
  aesthetics2 = aes(xmin = x_mean - 0.5 * x_sd,   
                    xmax = x_mean + 0.5 * x_sd,
                    ymin = y_mean - 0.5 * y_sd,
                    ymax = y_mean + 0.5 * y_sd,
                    colour=algorithm)
  basic_plot +
    geom_rect(aesthetics1, alpha=0.15) +
    geom_rect(aesthetics2, fill=NA)
}


colnames <- c('run', 'dataset', 'algorithm', 'metricName', 'sensitiveAttr', 'metricType', 'sensitiveType', 'metric')
df <- read.csv("results/sg_metrics/sg_metric_results.csv", header = FALSE, col.names = colnames, check.names = FALSE)
df$sensitiveAttr <- factor(df$sensitiveAttr, level=c(levels(df$sensitiveAttr), "Race"))
df$sensitiveAttr <- factor(df$sensitiveAttr, level=c(levels(df$sensitiveAttr), "sex-race"))
df$metric <- as.numeric(levels(df$metric))[df$metric]
df[which(df$dataset=='ricci'),]$sensitiveAttr <- 'Race'
df[which(df$dataset=='propublica-recidivism' & df$sensitiveAttr=='race-sex'),]$sensitiveAttr <- 'sex-race'
df[which(df$dataset=='propublica-violent-recidivism' & df$sensitiveAttr=='race-sex'),]$sensitiveAttr <- 'sex-race'
df <- subset(df, dataset != '')
df <- subset(df, metric <= 1)
df <- subset(df, algorithm %in% algos_to_plot)
df <- mutate(df, algorithm=recode(algorithm, ZafarFairness="Zafar")) # rename to Zafar for clarity
datasets <- unique(df$dataset)
algorithms <- unique(df$algorithm)
metricNames <- unique(df$metricName)
metricTypes <- unique(df$metricType)
sensitiveAttrs <- unique(df$sensitiveAttr)

plot_specific <- function(ss, x_var, y_var) {
  p <- plot_sensitivity(ss, x_var, y_var) + xlim(-0, 1) + ylim(-0, 1)
  return(p)
}

make_sensitivity_figure = function(ss, var1, var2) {
  df = ss %>%
    filter(algorithm %in% algos_to_plot) %>%
    mutate(algorithm=recode(algorithm, ZafarFairness="Zafar")) # rename to Zafar for clarity
  
  plot_sensitivity_old(df, var1, var2)
}
XVALS <- c('DIbinary', 'calibration+', 'CV')
YVALS <- c('accuracy', 'calibration-', 'BCR')
for (ds in datasets) {
  for (sens in sensitiveAttrs) {
    for (mt in metricTypes) {
      for (x_val in XVALS) {
        for (y_val in YVALS) {
          ss <- subset(df, dataset==ds & sensitiveAttr==sens & metricType==mt)
          if (nrow(ss) == 0) next
          p <- plot_specific(ss, x_val, y_val)
          p + ggtitle(paste(ds, sens))
          ggsave(paste0("figures/", paste(ds,sens,mt, x_val, y_val, "SG", sep="_"), ".png"), width=6, height=4, units='in')
        }
      }
    }
  }
}


make_subgroup_ratio_plot <- function(file) {
  df <- read.csv(file, check.names = FALSE)
  cols <- names(df)
  cols <- cols[which(unlist(lapply(cols, function(x){startsWith(x, "diff")})))]
  df <- df[cols]
  cols <- unlist(lapply(cols, gsub, pattern="diff:", replacement=""))
  cols1 <- unlist(lapply(cols, function(x){strsplit(x, "to")[[1]][1]}))
  cols2 <- unlist(lapply(cols, function(x){strsplit(x, "to")[[1]][2]}))
  metricName1 <- unlist(lapply(cols1, function(x){str_extract(x, "\\w+[-+]?$")}))
  metricName2 <- unlist(lapply(cols2, function(x){str_extract(x, "\\w+[-+]?$")}))
  sg1 <- mapply(gsub, pattern=paste0("-",metricName1), replacement="", x=cols1, USE.NAMES = FALSE)
  sg1 <- unlist(lapply(sg1, function(x){gsub("\\+", "", x)}))
  sg2 <- mapply(gsub, pattern=paste0("-",metricName2), replacement="", x=cols2, USE.NAMES = FALSE)
  sg2 <- unlist(lapply(sg2, function(x){gsub("\\+", "", x)}))
  
  res <- cbind(sg1, sg2, metricName1, (t(as.data.frame(df))))
  rownames(res) <- c()
  res <- as.data.frame(res)
  data <- melt(data = res, id.vars = c('sg1', 'sg2', 'metricName1'))
  data <- data[c('sg1', 'sg2', 'metricName1', 'value')]
  names(data) <- c('unprotected', 'protected', 'metricName', 'ratio')
  data$ratio <- as.numeric(data$ratio)
  p <- (ggplot(aes(y=ratio, x=metricName, fill = metricName), data=data)
     + geom_boxplot()
     + facet_wrap(vars(unprotected, protected), scales = "free_x", nrow = 2)
     + theme(axis.text.x = element_text(angle = 90, hjust = 1))
     )
  return (p)
}

for (ds in datasets) {
  for (sens in sensitiveAttrs) {
    for (mt in metricTypes) {
      tryCatch({
        p <- make_subgroup_ratio_plot(paste0("fairness/results/", paste(ds, sens, mt, sep="_"), ".csv"))
        p + ggtitle(paste(ds, "dataset,", "metric consistency across subgroups,", sens, mt))
        ggsave(paste0("figures/", paste("metricConsistency", ds,sens,mt, sep="_"), ".png"),
               width=10, height=7, units="in")
      }, error=function(err){
        print(err)
        print(paste("No results for", ds, sens, mt))
      })
    }
  }
}
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
make_corrplot <- function(ss) {
  ss <- ss[,c('algorithm', 'metricName', 'sensitiveType', 'metric')]
  cr <- cor(dcast(ss, algorithm+sensitiveType~metricName, mean)[3:12])
  #cr <- get_upper_tri(cr)
  cr <- reorder_cormat(cr)
  mtcr <- melt(cr,na.rm = TRUE)
  (ggplot(mtcr, aes(Var2, Var1, fill = value))
    + geom_tile(color = "white")
    + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation")
    + theme_minimal() # minimal theme
    + theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1))
    + theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))
    + coord_fixed())
}

for (ds in datasets) {
  for (sens in sensitiveAttrs) {
    for (mt in metricTypes) {
      tryCatch({
        ss <- subset(df, dataset==ds & sensitiveAttr==sens & metricType==mt)
        cp <- make_corrplot(ss)
        cp + ggtitle(paste(ds, "dataset,", sens, "attribute"))
        ggsave(paste0("figures/", paste("corrplot",ds,sens,mt, sep="_"), ".png"))
      }, error=function(err){
      print(err)
      })
    }
  }
  tryCatch({
    ss <- subset(df, dataset==ds)
    cp <- make_corrplot(ss)
    cp + ggtitle(paste(ds, "dataset,"))
    ggsave(paste0("figures/", paste("corrplot",ds, sep="_"), ".png"))
  }, error=function(err){
    print(err)
  })
}

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

for (ds in datasets) {
  ss <- subset(df, dataset==ds)
  ss <- ss[,c('metricName', 'sensitiveType', 'metric')]
  sdf <- ddply(ss, .(metricName, sensitiveType), function(x) {data.frame(sd = sd(x$metric))})
  df2 <- read.csv(paste0("SGratios_", ds, ".csv"), colClasses=c("NULL", NA, NA, NA))
  df3 <- merge(sdf, df2, by="sensitiveType")
  df3 <- df3[,c('metricName', 'sensitiveType', 'sd', 'ratio')]
  #df3g <- df3 %>% group_by(metricName)
  p <- (ggplot(df3, aes(y=sd, x=ratio, colour = metricName))
        + geom_point()
        + geom_smooth(method='lm', fill=NA, formula = y ~ poly(x, 2))
        + xlab("Probability mass")
        + ylab("standard deviation of metrics")
  )
  p + ggtitle(paste(ds, "dataset, probability mass of subgroups vs. standard deviation of metrics"))
  ggsave(paste0("figures/", paste0("SG_corr-size-SD_", ds, ".png")))
}
