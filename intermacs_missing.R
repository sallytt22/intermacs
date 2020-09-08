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

load(paste(g_prefix_ana,"ana_dta_pt.Rdata",sep = ""))
## see only LVAD
ana_dta_pt_1 <- ana_dta_pt %>%
  dplyr::filter(dtype == 1)

load(paste(g_prefix_ana,"ana_dta_pt.Rdata",sep = ""))
## see only RVAD
ana_dta_pt_2 <- ana_dta_pt %>%
  dplyr::filter(dtype == 2)

load(paste(g_prefix_ana,"ana_dta_pt.Rdata",sep = ""))
## see only BiVAD
ana_dta_pt_3 <- ana_dta_pt %>%
  dplyr::filter(dtype == 3)

load(paste(g_prefix_ana,"ana_dta_pt.Rdata",sep = ""))
## see only TAH
ana_dta_pt_4 <- ana_dta_pt %>%
  dplyr::filter(dtype == 4)

##-----------------------------------------------------------------------------
##                       MISSINGNESS
##-----------------------------------------------------------------------------
cov_cols <- c("height", "weight", "age", "gender", "cvp", "sys_bp", "dia_bp", "pas", "pad", "pcw", "wbc", "hemoglobin", "inr", "cholesterol")
mis_summary_1 <- NULL
mis_summary_2 <- NULL
mis_summary_3 <- NULL
mis_summary_4 <- NULL
for (i in 1:length(cov_cols)) {
    cur_cov_1 <- ana_dta_pt_1[[cov_cols[i]]]
    cur_cov_2 <- ana_dta_pt_2[[cov_cols[i]]]
    cur_cov_3 <- ana_dta_pt_3[[cov_cols[i]]]
    cur_cov_4 <- ana_dta_pt_4[[cov_cols[i]]]
    # proportion of missing value
    cur_mis_1 <- mean(is.na(cur_cov_1) | " " == cur_cov_1 | "." == cur_cov_1) 
    cur_mis_2 <- mean(is.na(cur_cov_2) | " " == cur_cov_2 | "." == cur_cov_2)
    cur_mis_3 <- mean(is.na(cur_cov_3) | " " == cur_cov_3 | "." == cur_cov_3)
    cur_mis_4 <- mean(is.na(cur_cov_4) | " " == cur_cov_4 | "." == cur_cov_4)
    mis_summary_1 <- c(mis_summary_1, cur_mis_1)
    mis_summary_2 <- c(mis_summary_2, cur_mis_2)
    mis_summary_3 <- c(mis_summary_3, cur_mis_3)
    mis_summary_4 <- c(mis_summary_4, cur_mis_4)
}

mis_summary <- data.frame(feature      = cov_cols,
                          missrate_LVAD = mis_summary_1,
                          missrate_RVAD = mis_summary_2,
                          missrate_BiVAD = mis_summary_3,
                          missrate_TAH = mis_summary_4)

## facet barplots
misslong <- gather(mis_summary, key="measure", value="value", c("missrate_LVAD", "missrate_RVAD", "missrate_BiVAD", "missrate_TAH"))

variable_names <- list(
  "missrate_LVAD"  = "missing rate (LVAD)",
  "missrate_RVAD"  = "missing rate (RVAD)",
  "missrate_BiVAD" = "missing rate (BiVAD)",
  "missrate_TAH"   = "missing rate (TAH)"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

facet <- ggplot(data = misslong, aes(x = feature, y = value)) +
           geom_bar(stat='identity', fill="forest green") +
           facet_wrap(~measure, ncol=1, scales="free_y", labeller=variable_labeller)

## save figure
ggsave(facet, filename = "missing_rate.png", path ="figure")