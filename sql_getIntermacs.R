0##----------------------------------------------------------------------------
## PROJECT:
##    INTERMACS DATA MANIPULATION II
## DATE:
##    MAY 2020
## DESCRIPTION:
##    CREATE ANALYSIS DATASETS FROM INTERMACS CSV FILES
##----------------------------------------------------------------------------
rm(list = ls())
options(error = recover, warn = 0)

require(dplyr)
require(tidyr)
require(cowplot)
require(ggplot2)
require(xtable)
require(sqldf)
require(stringr)

#
# A function to select baseline covariances and endpoints of patients from INTERMACS database
# 
#' @param g_n          the first n patients (22474 in total)
#' @param g_path       the working directory to work at
#' @param g_prefix     the working directory to access read-in csv files
#' @param g_prefix_ana the working directory to save output data
#' @param g_nrow       the number of rows of read-in data (change to -1 to read in all data)
#
getIntermacs <- function(g_n = 22474, g_path = "./", g_prefix = "IntermacsCSV/", g_prefix_ana = "AnalysisData/", g_nrow = -1) {
  
    ##-----------------------------------------------------------------------------
    ##                       LOAD CSV FILES
    ##-----------------------------------------------------------------------------
    
    ## read in data
    g_dta_patients <- read.csv(file = paste(g_path, g_prefix, "patients.csv", sep = ""), nrows = g_nrow)
    g_dta_events   <- read.csv(file = paste(g_path, g_prefix, "events.csv",   sep = ""), nrows = g_nrow)
    g_dta_follow   <- read.csv(file = paste(g_path, g_prefix, "followup.csv", sep = ""), nrows = g_nrow)
    g_dta_device   <- read.csv(file = paste(g_path, g_prefix, "device.csv",   sep = ""), nrows = g_nrow)
    
    # select first n patients
    size = g_n
    
    g_dta_patients <- g_dta_patients %>%
      filter(as.numeric(PUBLIC_ID) <= size)  
    
    g_dta_events <- g_dta_events %>%
      filter(as.numeric(PUBLIC_ID) <= size) 
    
    g_dta_follow <- g_dta_follow %>%
      filter(as.numeric(PUBLIC_ID) <= size) 
    
    g_dta_device <- g_dta_device %>%
      filter(as.numeric(PUBLIC_ID) <= size) 
    
    ##-----------------------------------------------------------------------------
    ##                       MANIPULATE DATA
    ##-----------------------------------------------------------------------------
    
    ## unique patient ids
    d_pids <- sqldf("select distinct public_id as id 
                     from g_dta_patients")
    
    ##-----------------------------------------------------------------------------
    ##                       BASELINE COVARIANCE 
    ##-----------------------------------------------------------------------------
    
    # sql statement
    s_sql_pt <- 'select distinct p0.id,
                                 p1.AGE_DEIDENT as age,
                                 p1.GENDER as gender,
                                 p1.HGT_CM as height,
                                 p1.WGT_KG as weight,
                                 p1.RACE_WHITE as caucasian,
                                 p1.RACE_AF_AMER as af_american,
                                 p1.primary_dgn as diagnosis,
                                 p1.device_strategy as indication,
                                 p1.PX_PROFILE as profile,
                                 p1.HR_RATE as heart_rate,
                                 p1.SYS_BP as sys_bp,             
                                 p1.DIA_BP as dia_bp,
                                 p1.PUL_SYS_PRES as sys_pap,
                                 p1.PUL_DIA_PRES as dia_pap,
                                 p1.RA_PRES as ra_pres,
                                 p1.PUL_WEDGE_PRES as pcwp,
                                 p1.BUN_MG_DL as bun,
                                 p1.CREAT_MG_DL as creatinine,
                                 p1.BILI_TOTAL_MG_DL as bilirubin_total,
                                 p1.SODIUM_MMOL_L as sodium,
                                 p1.INR as inr,
                                 p1.WBC_X10_3_UL as wbc,
                                 p1.PLATELET_X10_3_UL as platelet,
                                 p1.SGOT_AST as ast,
                                 p1.SGPT_ALT as alt,
                                 p1.CHOLESTEROL_MG_DL as cholesterol,
                                 p1.POTASSIUM_MEQ_L as potassium,
                                 p1.HEMOGLOBIN_G_DL as hemoglobin,
                                 p1.ALBUMIN_G_DL as albumin,
                                 p1.BNP_PG_ML as bnp,
                                 p1.MED_PRE_IMP_ACE_INHIBITORS as ace,
                                 p1.MED_PRE_IMP_BETA_BLOCKERS as beta_blk,
                                 p1.IV_INO_THERAPY as inotrope
                                 from d_pids p0 
                                 left join g_dta_patients p1 on (p0.id = p1.public_id)
                                 order by id'
    
    ana_dta_pt <- sqldf(s_sql_pt)
    
    # update isechemic etiology, btt, dt, ace_inhibitors and beta blockers
    ana_dta_pt <- ana_dta_pt %>%
      mutate(ischemic = (diagnosis == 8),
             ace_inhibitor = (ace >=1 & ace <=2),
             beta_blocker = (beta_blk >=1 & beta_blk <=2),
             btt  = (indication >=2 & indication <=5),
             dt   = indication == 6)
    
    # update INTERMACS profile
    ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "7", "4")
    ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "11", "6")
    ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "13", "7")
    ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "3", "2")
    ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "5", "3")
    ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "9", "5")
    
    ##-----------------------------------------------------------------------------
    ##                       OUTCOME/ENDPOINT
    ##-----------------------------------------------------------------------------
    
    ## outcome events
    ## time_of_death: 1-expected, 2-unexpected and 998-unknown
    s_sql_event <- 'select distinct p0.id,
                                    e.OPER_ID as oid,
                                    e.PATIENT_REPORT_ID as report_id,
                                    e.EVENTX as event_type,
                                    e.INT_EVT as int_event,
                                    p1.DEAD as dead,
                                    p1.TIMING_DEATH as time_of_death,
                                    p1.INT_DEAD as int_dead,
                                    e.PRIM_ADMIS_RSN as rehosp,
                                    e.AE_NEURO_CVA as mace_stroke
                                    from d_pids p0
                                    left join g_dta_events e on (p0.id = e.public_id)
                                    left join g_dta_patients p1 on (p0.id = p1.public_id)'
    
    ana_dta_event <- sqldf(s_sql_event)
    ana_dta_event <- ana_dta_event %>%
      mutate(RHF = (rehosp == 28),
             mace_myocardial = (rehosp == 6),
             ischemic_stroke = (mace_stroke == 1),
             hemorrhagic_stroke = (mace_stroke == 2))
    
    
    ## MACE 
    mace_event <- 'select distinct p1.*,
                                   m1.time as rhf_time,
                                   m2.time as myo_time,
                                   m3.time as ise_stroke_time,
                                   m4.time as hem_stroke_time,
                                   m5.time as dead_time
                                   from ana_dta_event as p1
                                   left join (select id, min(int_event) as time
                                   from ana_dta_event 
                                   where RHF = 1
                                   group by id)
                                   m1 on (p1.id = m1.id)
                                   left join (select id, min(int_event) as time
                                   from ana_dta_event 
                                   where mace_myocardial = 1
                                   group by id)
                                   m2 on (p1.id = m2.id)
                                   left join (select id, min(int_event) as time
                                   from ana_dta_event 
                                   where ischemic_stroke = 1
                                   group by id)
                                   m3 on (p1.id = m3.id)
                                   left join (select id, min(int_event) as time
                                   from ana_dta_event 
                                   where hemorrhagic_stroke = 1
                                   group by id)
                                   m4 on (p1.id = m4.id)
                                   left join (select id, min(int_event) as time
                                   from ana_dta_event 
                                   where dead = 1
                                   group by id)
                                   m5 on (p1.id = m5.id)'
    
    ana_dta_event <- sqldf(mace_event)
    
    
    
    ##-----------------------------------------------------------------------------
    ##                       FOLLOW_UP w/ FORM_ID
    ##-----------------------------------------------------------------------------
    
    ## INTOP1FU: Interval from First Device to this Followup (months) 
    s_sql_follow <-  'select distinct p0.id as id,
                                      f.OPER_ID as operation_id, 
                                      f.FORM_ID as form_id,
                                      f.INTOP1FU as follow_mths,
                                      365.25/12*f.INTOP1FU as follow_days,
                                      f.SIX_MIN_WALK as six_min_walk
                                      from d_pids p0
                                      left join g_dta_follow f on (p0.id = f.public_id)'
    
    ana_dta_follow <- sqldf(s_sql_follow)
    
    
    
    ##-----------------------------------------------------------------------------
    ##                       OUTPUT DATA
    ##-----------------------------------------------------------------------------
    
    ## output baseline covariances table
    write.csv(ana_dta_pt, file = paste(g_path, g_prefix_ana, "baselines.csv", sep = ""), row.names = FALSE)
    save(ana_dta_pt, file = paste(g_path, g_prefix_ana, "baselines.Rdata", sep = ""))
    
    ## output outcomes table
    write.csv(ana_dta_event, file = paste(g_path, g_prefix_ana, "outcomes.csv", sep = ""), row.names = FALSE)
    save(ana_dta_event, file = paste(g_path, g_prefix_ana, "outcomes.Rdata", sep = ""))
    
    ## output follow-up table
    write.csv(ana_dta_follow, file = paste(g_path, g_prefix_ana, "follow.csv", sep = ""), row.names = FALSE)
    save(ana_dta_follow, file = paste(g_path, g_prefix_ana, "follow.Rdata", sep = ""))
}


getIntermacs(g_path = "clinical_data/")

# adverse events
# evt <- sqldf('select distinct e.id
#              from ana_dta_event e
#              where (e.event_type =  "Bleeding")')
  


