
    
    
##----------------------------------------------------------------------------
##                GET target_stats LIST
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
    
  
    

  


# 50% quantile = median
# target_lists$age <- list(type = 'quants', quants = rbind(c(quants = 0.5, x_quants = 55)))










