##----------------------------------------------------------------------------
## PROJECT:
##    INTERMACS DATA MANIPULATION I
## DATE:
##    MAR 2020
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


##-----------------------------------------------------------------------------
##                       LOAD CSV FILES
##-----------------------------------------------------------------------------

## change this to the path where the csv files are
setwd("/Users/sally/clinical_data/")

g_prefix     <- "IntermacsCSV/"
g_prefix_ana <- "AnalysisData/"

## change to -1 to read in all data
g_nrow   <- -1

## read in data
g_dta_patients <- read.csv(file = paste(g_prefix, "patients.csv", sep = ""), nrows = g_nrow)
g_dta_events   <- read.csv(file = paste(g_prefix, "events.csv",   sep = ""), nrows = g_nrow)
g_dta_follow   <- read.csv(file = paste(g_prefix, "followup.csv", sep = ""), nrows = g_nrow)
g_dta_device   <- read.csv(file = paste(g_prefix, "device.csv",   sep = ""), nrows = g_nrow)

# select first 100 patients
g_dta_patients <- g_dta_patients %>%
filter(as.numeric(PUBLIC_ID) <= 100)  

g_dta_events <- g_dta_events %>%
  filter(as.numeric(PUBLIC_ID) <= 100) 

g_dta_follow <- g_dta_follow %>%
  filter(as.numeric(PUBLIC_ID) <= 100) 

g_dta_device <- g_dta_device %>%
  filter(as.numeric(PUBLIC_ID) <= 100) 
##-----------------------------------------------------------------------------
##                       MANIPULATE DATA
##-----------------------------------------------------------------------------

## unique patient ids
d_pids <- sqldf("select distinct public_id as id from g_dta_patients")


## patients baseline data
s_sql_pt <- 'select distinct p0.id,
             p1.AGE_DEIDENT as age,
             p1.IMPL_YR as implant_yr,
             p1.QRTR as implant_qrtr,
             p1.gender as gender,
             p1.hgt_cm as height,
             p1.wgt_kg as weight,
             p1.primary_dgn as diagnosis,
             p1.device_strategy as strategy,
             p1.PX_PROFILE as profile,
             p1.LVEF as lvef,
             p1.CV_PRES as cvp,
             p1.SYS_BP as sys_bp,
             p1.DIA_BP as dia_bp,
             p1.PUL_SYS_PRES as pas,
             p1.PUL_DIA_PRES as pad,
             p1.PUL_WEDGE_PRES as pcwp,
             p1.INR as inr,
             p1.HEMOGLOBIN_G_DL as hemoglobin,
             p1.CHOLESTEROL_MMOL_L as cholesterol,
             p1.WBC_X10_3_UL as wbc,
             p1.MED_PRE_IMP_ACE_INHIBITORS as ace,
             d.DEVICE_TY as dtype
             from d_pids p0 
             left join g_dta_patients p1 on (p0.id = p1.public_id)
             left join g_dta_device d on (p0.id = d.public_id)'
             

ana_dta_pt <- sqldf(s_sql_pt)
ana_dta_pt <- ana_dta_pt %>%
  mutate(ischemic = (diagnosis == 8),
         ace_inhibitor = (ace >=1 & ace <=2),
         btt  = (strategy >=2 & strategy <=5),
         dt   = strategy == 6)

ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "7", "4")
ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "11", "6")
ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "13", "7")
ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "3", "2")
ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "5", "3")
ana_dta_pt$profile <- str_replace_all(ana_dta_pt$profile, "9", "5")


## outcome events
s_sql_event <- 'select distinct p0.id,
                e.OPER_ID as oid,
                e.PATIENT_REPORT_ID as report_id,
                e.EXPLANT as explant,
                e.PRIM_ADMIS_RSN as mace,
                e.AE_NEURO_CVA as mace_stroke,
                e.INT_EVT as int_evt,
                e.INTOP1AE as ae_int,
                p1.DEAD as dead,
                p1.INT_DEAD as survive_mon,
                p1.PRIMARY_COD as COD,
                p1.PRIMARY_COD_CANCER as COD_cancer
                from d_pids p0
                left join g_dta_events e on (p0.id = e.public_id)
                left join g_dta_patients p1 on (p0.id = p1.public_id)
                left join g_dta_follow f on (p0.id = f.public_id and 
                                             e.OPER_ID = f.OPER_ID)'

ana_dta_event <- sqldf(s_sql_event)
ana_dta_event <- ana_dta_event %>%
  mutate(mace_rhf = (mace == 28),
         mace_myocardial = (mace == 6),
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
               left join (select id, min(ae_int) as time
                          from ana_dta_event 
                          where mace_rhf = 1
                          group by id)
                          m1 on (p1.id = m1.id)
               left join (select id, min(ae_int) as time
                          from ana_dta_event 
                          where mace_myocardial = 1
                          group by id)
                          m2 on (p1.id = m2.id)
               left join (select id, min(ae_int) as time
                          from ana_dta_event 
                          where ischemic_stroke = 1
                          group by id)
                          m3 on (p1.id = m3.id)
               left join (select id, min(ae_int) as time
                          from ana_dta_event 
                          where hemorrhagic_stroke = 1
                          group by id)
                          m4 on (p1.id = m4.id)
               left join (select id, min(ae_int) as time
                          from ana_dta_event 
                          where dead = 1
                          group by id)
                          m5 on (p1.id = m5.id)'

ana_dta_event <- sqldf(mace_event)


##-----------------------------------------------------------------------------
##                      DATA v.s. FLOW CHART
##-----------------------------------------------------------------------------

## dead patients
dead_pids <- sqldf('select distinct p0.id,
                    p1.IMPL_YR as implant_yr,
                    p1.DEAD as dead,
                    p1.PRIMARY_COD as primary_cod,
                    p1.INT_DEAD as int_dead,
                    p1.OP as op_count,
                    p1.DISCHARGE_STATUS as discharge_status,
                    p1.LOS as int_discharge,
                    p1.OP as op_count
                    from d_pids p0
                    left join g_dta_patients p1 on (p0.id = p1.public_id)
                    where dead = 1 
                    order by id')
length(dead_pids$id)


## discharged alive with a device in place (i.e. discharge_status = 1)
d1_pids <- sqldf('select distinct p0.id,
                   p1.DEAD as dead,
                   p1.INT_DEAD as int_dead,
                   p1.DISCHARGE_STATUS as discharge_status,
                   p1.LOS as int_discharge,
                   f.FORM_ID as form_id,
                   f.INT_AFOL as int_act_fol,
                   f.OPER_ID as oper_id,
                   from d_pids p0
                   left join g_dta_patients p1 on (p0.id = p1.public_id)
                   left join g_dta_follow f on (p0.id = f.public_id)
                   where (dead = 1 and discharge_status = 1) 
                   order by id')

write.csv(d1_pids, "/Users/sally/clinical_data/AnalysisData/d1_pids.csv", row.names = FALSE)



## rehosp
rehosp_pids <- sqldf('select distinct p0.id,
                   p1.DEAD as dead,
                 p1.INT_DEAD as int_dead,
                 p1.DISCHARGE_STATUS as discharge_status,
                 p1.LOS as int_discharge,
                 e.REHOSPITALIZATION as rehosp,
                 e.INTOP1AE as int_event,
                 e.EVENTX as evtx,
                 e.INT_REH as len_rhs,
                 e.PRIM_ADMIS_RSN as rehosp_rsn
                 from d_pids p0
                 left join g_dta_patients p1 on (p0.id = p1.public_id)
                 left join g_dta_events e on (p0.id = e.public_id)
                 where (dead = 1 and discharge_status = 1 and rehosp = "Y") 
                 order by id')

write.csv(rehosp_pids, "/Users/sally/clinical_data/AnalysisData/rehosp_pids.csv", row.names = FALSE)


## patient died during the implant hospitalization (i.e. discharge_status = 2)



## variable -- EVENTX
evt <- sqldf('select distinct e.EVENTX,
              e.OPER_ID
              from g_dta_events e
              where (e.PUBLIC_ID =  "PUB000008")')

evt_x <- sqldf('select distinct e.EVENTX
             from g_dta_events e')

## variable -- FORM_ID
ff <- sqldf('select distinct f.PUBLIC_ID,
              f.FORM_ID
             from g_dta_follow f
             where (f.PUBLIC_ID =  "PUB000009")')

form_4 <- sqldf('select distinct f.*
            from g_dta_follow f
            where (f.FORM_ID =  4)')


device <- sqldf('select distinct d.PUBLIC_ID, 
                 d.OPER_ID as oper_id,
                 d.DEV_COUNTER as dev_num,
                 d.SIX_MIN_WALK
                 from g_dta_device d')

pp <- sqldf('select distinct p1.PUBLIC_ID,
                 p1.SIX_MIN_WALK
                from g_dta_patients p1')

dev_follow <- sqldf('select distinct d.PUBLIC_ID,
                    d.OPER_ID as oper_id,
                    d.SIX_MIN_WALK as d_six,
                    f.SIX_MIN_WALK as f_six,
                    f.FORM_ID as form_id
                    from g_dta_device d
                    left join g_dta_follow f on (d.PUBLIC_ID = f.PUBLIC_ID)')

pp_dev <- sqldf('select distinct p1.PUBLIC_ID,
                    p1.SIX_MIN_WALK as p1_six,
                    d.SIX_MIN_WALK as d_six
                    from g_dta_device p1
                    left join g_dta_device d on (p1.PUBLIC_ID = d.PUBLIC_ID)')




## patient has device exchange (i.e. discharge_status = 5)
d5_pids <- sqldf('select distinct p0.id,
                  p1.DEAD as dead,
                  p1.INT_DEAD as int_dead,
                  p1.DISCHARGE_STATUS as discharge_status,
                  p1.LOS as int_discharge, 
                  f.FORM_ID as form_id,
                  f.INT_AFOL as int_act_fol
                  from d_pids p0
                  left join g_dta_patients p1 on (p0.id = p1.public_id)
                  left join g_dta_follow f on (p0.id = f.public_id)
                  where (dead = 1 and discharge_status = 5) 
                  order by id')





##-----------------------------------------------------------------------------
##                      TEST
##-----------------------------------------------------------------------------

## test 
temp_pids <- sqldf("select distinct public_id as id,
                   AGE_DEIDENT as age,
                   OPER_ID as oid,   
                   OP1EVTID as oid1,
                   OP2EVTID as oid2,
                   IMPL_YR as implant_yr,
                   INT_DEAD as int_dead
                   from g_dta_patients order by id")

e_pids <-  sqldf("select distinct public_id as id,
                  OPER_ID as oid, 
                  PATIENT_REPORT_ID as rid,
                  AE_DEV_THR_EVNT_SYM_AB_PMP_PARAM as pump
                  from g_dta_events order by id")

device_pids <-  sqldf("select distinct public_id as id,
                                  OPER_ID as oid, 
                                  DEV_COUNTER as dcount
                                  from g_dta_device order by id")

follow_pids <-  sqldf("select distinct public_id as id,
                                  OPER_ID as operation_id, 
                                  FORM_ID as form_id,
                                  INTOP1FU as follow_mths,
                                  365.25/12*INTOP1FU as follow_days,
                                  SIX_MIN_WALK as six_min_walk
                                  from g_dta_follow order by id")


# quality of life -- ED-5D
eq_pids <- sqldf("select distinct public_id as id,
                                  OPER_ID as oid, 
                                  FORM_ID as fid,
                                  EQ_INDEX as eq5d,
                                  365.25/12*INTOP1FU as followup_mon
                                  from g_dta_follow order by id")


## output data
write.csv(ana_dta_pt, "/Users/sally/clinical_data/AnalysisData/baseline.csv", row.names = FALSE)
write.csv(ana_dta_event, "/Users/sally/clinical_data/AnalysisData/outcome.csv", row.names = FALSE)
save(ana_dta_pt,    file = paste(g_prefix_ana, "ana_dta_pt.Rdata",    sep = ""))
save(ana_dta_event, file = paste(g_prefix_ana, "ana_dta_event.Rdata", sep = ""))

