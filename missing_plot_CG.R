##----------------------------------------------------------------------------
## PROJECT:
##    INTERMACS DATA MANIPULATION
## DATE:
##    MAR 2020
## DESCRIPTION:
##    MISSING DATA PRESENTATION
##----------------------------------------------------------------------------

rm(list = ls())
options(error = recover, warn = 0)

require(dplyr)
require(tidyr)
require(cowplot)
require(ggplot2)
require(xtable)
require(sqldf)

##-----------------------------------------------------------------------------
##                       LOAD ANALYSIS DATASETS
##-----------------------------------------------------------------------------
setwd("/Users/sally/clinical_data/")

g_prefix_ana <- "AnalysisData/"

for (k in 1:4) {
  load(paste(g_prefix_ana,"ana_dta_pt.Rdata",sep = ""))
  curr <- NULL
  ## see only LVAD
  curr <- ana_dta_pt %>%
    dplyr::filter(dtype == k)

    ##-----------------------------------------------------------------------------
    ##                       MISSINGNESS
    ##-----------------------------------------------------------------------------
  
    cov_cols <- c("height", "weight", "age", "gender", "cvp", "sys_bp", "dia_bp", "pas", "pad", "pcw", "wbc", "hemoglobin", "inr", "cholesterol")
    mis_col_name <- c("LVAD", "RVAD", "BiVAD", "TAH")
    mis_summary <- data.frame(feature = cov_cols)
    miss <- NULL
    for (i in 1:length(cov_cols)) {
      cur_cov <- curr[[cov_cols[i]]]
      cur_mis <- mean(is.na(cur_cov) | " " == cur_cov | "." == cur_cov) # proportion of missing value
      miss <- c(miss, cur_mis)
    }
    mis_summary$mis_col_name[k] <- miss
}



## facet barplots
misslong <- gather(mis_summary, key="measure", value="value", c("missrate_LVAD","missrate_BiVAD"))

ggplot(data = misslong, aes(x = feature, y = value)) +
  geom_bar(stat='identity', fill="forest green") +
  facet_wrap(~measure,ncol=1)

