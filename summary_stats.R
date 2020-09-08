require(dplyr)
require(tidyr)
require(cowplot)
require(ggplot2)
require(xtable)
require(sqldf)
require(stringr)

require(rwetools)

##-----------------------------------------------------------------------------
##                       LOAD INTERMACS DATA
##-----------------------------------------------------------------------------

g_path = "clinical_data/"
g_prefix = "IntermacsCSV/"
g_prefix_ana = "AnalysisData/"
g_nrow = -1

g_dta_patients <- read.csv(file = paste(g_path, g_prefix, "patients.csv", sep = ""), nrows = g_nrow)
g_dta_events <- read.csv(file = paste(g_path, g_prefix, "events.csv", sep = ""), nrows = g_nrow)


d_pids <- sqldf("select distinct public_id as id 
                     from g_dta_patients")


##-----------------------------------------------------------------------------
##                       GET dta_ext DATAFRAME 
##-----------------------------------------------------------------------------

# sql statement
s_sql_pt <- 'select distinct p0.id,
                              p1.IMPL_YR as implant_year,
                              p1.AGE_DEIDENT as age,
                              p1.GENDER as gender,
                              SQRT(p1.HGT_CM * p1.WGT_KG) / 60 as bsa,
                              10000 * p1.WGT_KG / p1.HGT_CM / p1.HGT_CM as bmi,
                              p1.CV_PRES as cvp,
                              p1.SYS_BP as sys_bp,             
                              p1.DIA_BP as dia_bp,
                              p1.PUL_SYS_PRES as sys_pap,
                              p1.PUL_DIA_PRES as dia_pap,
                              p1.PUL_WEDGE_PRES as pcwp,
                              p1.INR as inr,
                              p1.WBC_X10_3_UL as wbc,
                              p1.PLATELET_X10_3_UL as platelet,
                              p1.DEAD as dead,
                              p1.INT_DEAD as int_dead,
                              CASE WHEN p1.MED_PRE_IMP_ACE_INHIBITORS = 1 OR p1.MED_PRE_IMP_ACE_INHIBITORS = 2 THEN 1 
                                   WHEN p1.MED_PRE_IMP_ACE_INHIBITORS = 3 THEN 0 ELSE -1 END as ace
                              from d_pids p0 
                              left join g_dta_patients p1 on (p0.id = p1.public_id)
                              order by id'

dta_ext <- sqldf(s_sql_pt) 

curr <- NULL
curr <- dta_ext
curr[curr == '.'] <- NA
curr[curr == ' '] <- NA
curr[curr == -1] <- NA
curr <- na.omit(curr)
dta_ext <- curr

s_sql_pt_2 <- 'select p2.*,
CASE WHEN dead = 1 AND int_dead <= 13 THEN 1 ELSE 0 END as dead_6mths
FROM dta_ext p2
ORDER BY id'

dta_ext <- sqldf(s_sql_pt_2)
dta_ext$dead <- NULL
dta_ext$int_dead <- NULL
dta_ext <- na.omit(dta_ext)

s_sql_pt_3 <- 'select p2.*,
                      o1.transplant as transplant_6_mths,
                      o2.exchange as exchange_6_mths,
                      o3.rhf as rhf_6_mths
                      FROM dta_ext p2
                      LEFT JOIN (select e.public_id as id, 
                                       CASE WHEN e.EVENTX = "Explant: Transplant" THEN 1 ELSE 0 END as transplant
                                       FROM g_dta_events e
                                       WHERE e.INT_EVT <= 13
                                       GROUP BY id) o1 on (o1.id = p2.id)
                      LEFT JOIN (select e.public_id as id, 
                                       CASE WHEN e.EVENTX = "Explant: Exchange" THEN 1 ELSE 0 END as exchange
                                        FROM g_dta_events e
                                        WHERE e.INT_EVT <= 13
                                        GROUP BY id) o2 on (o2.id = p2.id)
                      LEFT JOIN (select e.public_id as id, 
                                 CASE WHEN e.EVENTX = "Right Heart Failure v3.0" THEN 1 ELSE 0 END as rhf
                                 FROM g_dta_events e
                                 WHERE e.INT_EVT <= 13
                                 GROUP BY id) o3 on (o3.id = p2.id)'
dta_ext <- sqldf(s_sql_pt_3)
dta_ext <- na.omit(dta_ext)

dta_ext <- dta_ext %>%
  filter(gender %in% c("M", "F")) %>%
  mutate(gender   = factor(gender),
         age      = as.numeric(age),
         cvp      = as.numeric(cvp),
         sys_bp   = as.numeric(sys_bp),
         dia_bp   = as.numeric(dia_bp),
         sys_pap  = as.numeric(sys_pap),
         dia_pap  = as.numeric(dia_pap),
         pcwp     = as.numeric(pcwp),
         inr      = as.numeric(inr),
         wbc      = as.numeric(wbc),
         platelet = as.numeric(platelet))

dta_ext$exchange_6_mths <- NULL

##----------------------------------------------------------------------------
##                GET target_stats from three studies
##----------------------------------------------------------------------------
study_1 <- list(study_name    = "HM3",
                patient_size  = 50,
                study_outcome = c(dead_6_mths       = 0.08, 
                                  transplant_6_mths = 0.04, 
                                  rhf_6_mths        = 0.1),
                target_summary = list(age      = list(type   = 'continuous', 
                                                      mean   = 5.89, 
                                                      sd     = 1.35, 
                                                      ex2    = 36.5146,
                                                      scale  = 0.1),
                                      
                                      gender   = list(type   = 'discrete', 
                                                      values = c('M','F'), 
                                                      probs  = c(0.9, 0.1)),
                                      
                                      bsa      = list(type   = 'continuous', 
                                                      mean   = 2, 
                                                      sd     = 0.2, 
                                                      ex2    = 4.04),
                                      
                                      bmi      = list(type   = 'continuous', 
                                                      mean   = 2.68, 
                                                      sd     = 0.43, 
                                                      ex2    = 7.4749,
                                                      scale  = 0.1),
                                      
                                      cvp      = list(type   = 'continuous', 
                                                      mean   = 0.99, 
                                                      sd     = 0.56, 
                                                      ex2    = 1.2937,
                                                      scale  = 0.1),
                                      
                                      sys_bp   = list(type   = 'continuous', 
                                                      mean   = 1.047, 
                                                      sd     = 0.114, 
                                                      ex2    = 1.109205,
                                                      scale  = 0.01),
                                      
                                      dia_bp   = list(type  = 'continuous', 
                                                      mean   = 6.47, 
                                                      sd     = 0.94, 
                                                      ex2    = 42.7445,
                                                      scale  = 0.1),
                                      
                                      sys_pap  = list(type  = 'continuous', 
                                                      mean  = 5.08, 
                                                      sd    = 1.86, 
                                                      ex2   = 29.266,
                                                      scale = 0.1),
                                      
                                      dia_pap  = list(type  = 'continuous', 
                                                      mean  = 2.36, 
                                                      sd    = 0.93, 
                                                      ex2   = 6.4345,
                                                      scale = 0.1),
                                      
                                      pcwp     = list(type  = 'continuous', 
                                                      mean  = 2.24, 
                                                      sd    = 0.87, 
                                                      ex2   = 5.7745,
                                                      scale = 0.1),
                                      
                                      wbc      = list(type = 'continuous', 
                                                      mean  = 1.32, 
                                                      sd    = 0.2, 
                                                      ex2   = 1.7824,
                                                      scale = 0.1),
                                      
                                      platelet = list(type  = 'continuous', 
                                                      mean  = 2.31, 
                                                      sd    = 0.693, 
                                                      ex2   = 5.816349,
                                                      scale = 0.01),
                                      
                                      inr      = list(type = 'continuous',
                                                      mean = 1.3, 
                                                      sd   = 0.6, 
                                                      ex2  = 2.05),
                                      
                                      ace      = list(type   = 'discrete', 
                                                      values = c('0','1'), 
                                                      probs  = c(0.58, 0.42))
                )
)

study_2 <- list(study_name    = "HM2", 
                patient_size  = 133,
                study_outcome = c(dead_6_mths       = 0.19, 
                                  transplant_6_mths = 0.42, 
                                  rhf_6_mths        = 0.17),
                target_summary = list(age      = list(type   = 'continuous', 
                                                      mean   = 5.01, 
                                                      sd     = 1.31, 
                                                      ex2    = 26.8162,
                                                      scale  = 0.1),
                                      
                                      gender   = list(type   = 'discrete', 
                                                      values = c('M','F'), 
                                                      probs  = c(0.7895, 0.2105)),
                                      
                                      bsa      = list(type   = 'continuous', 
                                                      mean   = 2, 
                                                      sd     = 0.3, 
                                                      ex2    = 4.09),
                                      
                                      bmi      = list(type   = 'continuous', 
                                                      mean   = 2.68, 
                                                      sd     = 0.59, 
                                                      ex2    = 7.5305,
                                                      scale  = 0.1),
                                      
                                      cvp      = list(type   = 'continuous', 
                                                      mean   = 1.35, 
                                                      sd     = 0.78, 
                                                      ex2    = 2.4309,
                                                      scale  = 0.1),
                                      
                                      sys_bp   = list(type   = 'continuous', 
                                                      mean   = 9.58, 
                                                      sd     = 1.46, 
                                                      ex2    = 93.908,
                                                      scale  = 0.1),
                                      
                                      dia_bp   = list(type  = 'continuous', 
                                                      mean   = 6.17, 
                                                      sd     = 1.13, 
                                                      ex2    = 39.3458,
                                                      scale  = 0.1),
                                      
                                      sys_pap  = list(type  = 'continuous', 
                                                      mean  = 5.3, 
                                                      sd    = 1.41, 
                                                      ex2   = 30.0781,
                                                      scale = 0.1),
                                      
                                      dia_pap  = list(type  = 'continuous', 
                                                      mean  = 2.82, 
                                                      sd    = 0.88, 
                                                      ex2   = 8.7268,
                                                      scale = 0.1),
                                      
                                      pcwp     = list(type  = 'continuous', 
                                                      mean  = 2.61, 
                                                      sd    = 0.79, 
                                                      ex2   = 7.4362,
                                                      scale = 0.1),
                                      
                                      wbc      = list(type = 'continuous', 
                                                      mean = 8.9, 
                                                      sd   = 3.2, 
                                                      ex2  = 89.45),
                                      
                                      platelet = list(type  = 'continuous', 
                                                      mean  = 2.28, 
                                                      sd    = 0.86, 
                                                      ex2   = 5.9380,
                                                      scale = 0.01),
                                      
                                      inr      = list(type = 'continuous',
                                                      mean = 1.3, 
                                                      sd   = 0.4, 
                                                      ex2  = 1.85),
                                      
                                      ace      = list(type   = 'discrete', 
                                                      values = c('0','1'), 
                                                      probs  = c(0.7, 0.3))
                )
)

study_3 <- list(study_name    = "HeartWare HVAD", 
                patient_size  = 140,
                study_outcome = c(dead_6_mths       = 0.043, 
                                  transplant_6_mths = 0.27, 
                                  rhf_6_mths        = 0.193),
                target_summary = list(age      = list(type   = 'continuous', 
                                                      mean   = 5.33, 
                                                      sd     = 1.03, 
                                                      ex2    = 29.4698,
                                                      scale  = 0.1),
                                      
                                      gender   = list(type   = 'discrete', 
                                                      values = c('M','F'), 
                                                      probs  = c(0.7214, 0.2786)),
                                      
                                      bsa      = list(type   = 'continuous', 
                                                      mean   = 2.06, 
                                                      sd     = 0.28, 
                                                      ex2    = 4.322),
                                      
                                      bmi      = list(type   = 'continuous', 
                                                      mean   = 2.86, 
                                                      sd     = 0.61, 
                                                      ex2    = 8.5517,
                                                      scale  = 0.1),
                                      
                                      cvp      = list(type   = 'continuous', 
                                                      mean   = 1.08, 
                                                      sd     = 0.33, 
                                                      ex2    = 1.2753,
                                                      scale  = 0.1),
                                      
                                      sys_bp   = list(type   = 'continuous', 
                                                      mean   = 1.04, 
                                                      sd     = 0.16, 
                                                      ex2    = 1.1072,
                                                      scale  = 0.01),
                                      
                                      dia_bp   = list(type   = 'continuous', 
                                                      mean   = 6.4, 
                                                      sd     = 1.1, 
                                                      ex2    = 42.17,
                                                      scale  = 0.1),
                                      
                                      sys_pap  = list(type  = 'continuous', 
                                                      mean  = 4.9, 
                                                      sd    = 1.5, 
                                                      ex2   = 26.26,
                                                      scale = 0.1),
                                      
                                      dia_pap  = list(type  = 'continuous', 
                                                      mean  = 2.5, 
                                                      sd    = 0.9, 
                                                      ex2   = 7.06,
                                                      scale = 0.1),
                                      
                                      pcwp     = list(type  = 'continuous', 
                                                      mean  = 2.3, 
                                                      sd    = 0.9, 
                                                      ex2   = 6.10,
                                                      scale = 0.1),
                                      
                                      wbc      = list(type = 'continuous', 
                                                      mean = 7.5, 
                                                      sd   = 2.5, 
                                                      ex2  = 62.5),
                                      
                                      platelet = list(type  = 'continuous', 
                                                      mean  = 2.16, 
                                                      sd    = 0.76, 
                                                      ex2   = 5.2432,
                                                      scale = 0.01),
                                      
                                      inr      = list(type = 'continuous',
                                                      mean = 1.3, 
                                                      sd   = 0.4, 
                                                      ex2  = 1.85),
                                      
                                      ace      = list(type   = 'discrete', 
                                                      values = c('0','1'), 
                                                      probs  = c(0.2320, 0.3906))
                )
)    

study <- list(study_1 = study_1, 
              study_2 = study_2, 
              study_3 = study_3)

target_stats <- study$study_1$target_summary



##-----------------------------------------------------------------------------
##                       GET summarized statistics for Intermacs 
##-----------------------------------------------------------------------------
intermacs_stats <- rwe_extract_stats(target_stats, dta_ext)


##-----------------------------------------------------------------------------
##                       OUTPUT DATA
##-----------------------------------------------------------------------------
save(dta_ext, study, intermacs_stats, file = paste(g_path, g_prefix_ana, 'summarized_stats.RData', sep = ""))





