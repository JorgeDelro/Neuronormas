# Neuronorma function

library(tidyverse)

score_test = list(Q_COG_TMT_A_PRE = "Trail making test a", 
                             Q_COG_TMT_B_PRE = "Trail making test b",
                             Q_COG_WAIS_FORWARD_PRE = "Digit span forward",
                             Q_COG_WAIS_BACKWARD_PRE = "Digit span backward",
                             Q_COG_STROOP_W_TP_PRE = "Stroop color-word interference test (word)",
                             Q_COG_STROOP_C_TP_PRE = "Stroop color-word interference test (color)",
                             Q_COG_STROOP_WC_TP_PRE = "Stroop color-word interference test (interference)",
                             Q_COG_COWAT_ANIMAL_PRE = "COWAT_animal")

sex <- NULL

neuronorma <- function(score_test, age, education_years, sex, db){
  
  # Dataframe with data
  # db <- data.frame(score = score, age = age, education_years = education_years)
  
  # If age OR educational level is NA -> remove case
  
  # Age 
  db <- db[complete.cases(db[, age]), ]
  # Educational level
  db <- db[complete.cases(db[, education_years]), ]
  
  
  # Sanity Check
  for (i in 1:nrow(db)) {
    if(db[i, education_years] > 20) { 
      db[i, education_years] = 20 }
  }
  
  # If fruit or kitchen test are selected
  # the var sex must be included
  for (i in 1:length(score_test)) {
    if(score_test[[i]] == "COWAT_fruit" && is.null(sex)) 
      {print("Must include the variable sex")}
  }
  
  
  
  
  # Temporary data frame
  res_new <- data.frame(temp = nrow(db))
  
  for (i in 1:length(score_test)) {
    test <- score_test[[i]]
    score <- pull(db[,names(score_test)[i]])
    res <- calculate_score(score = score, 
                           test = test, 
                           age = pull(db[age]), 
                           education_years = pull(db[education_years]))
    res_new <- cbind(res_new, res)
  }
  
  
  res_new <- cbind(db, res_new[-1])
  
  return(res_new)
  
}

calculate_score <- function(score, 
                            test, 
                            age, 
                            education_years
) {
  
  
  if(test == "Trail making test a") { # TMTA
    res <- tmta(score = score,
                age = age,
                education_years = education_years)
    
    
    
  }
  if(test == "Trail making test b") { # TMTB
    res <- tmtb(score = score,
                age = age,
                education_years = education_years)
    
    
  } 
  if(test == "Symbol digit modalities test") { # SDMT
    res <- SDMT(score = score,
                age = age,
                education_years = education_years)
    
    
    
  } 
  if(test == "Letter-number sequencing raw score") { # LNS_RAW
    res <- LNS_RAW(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "Letter-number sequencing last item score") { # LNS_LI
    res <- LNS_LI(score = score,
                  age = age,
                  education_years = education_years)
    
    
    
  } 
  if(test == "Digit span forward") { # DS_FORWARD
    res <- ds_forward(score = score,
                      age = age,
                      education_years = education_years)
    
    
  } else if(test == "Digit span backward") { # DS_BACKWARD
    res <- ds_backward(score = score,
                       age = age,
                       education_years = education_years)
    
    res_new <- cbind(res_new, res)
    
  } 
  if(test == "Coris blocks forward raw score") { # CB_F_RAW
    res <- CB_F_RAW(score = score,
                    age = age,
                    education_years = education_years)
    
    
    
  } 
  if(test == "Coris blocks forward last item score") { # CB_F_LI
    res <- CB_F_LI(score = db$score,
                   age = db$age,
                   education_years = db$education_years)
    
    
  } 
  if(test == "Coris blocks backward raw score") { # CB_B_RAW
    res <- CB_B_RAW(score = score,
                    age = age,
                    education_years = education_years)
    
    
  } 
  if(test == "Coris blocks backward last item score") { # CB_B_LI
    res <- CB_B_LI(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "Stroop color-word interference test (word)") { # word
    res <- word(score = score,
                age = age,
                education_years = education_years)
    
    
  } 
  if(test == "Stroop color-word interference test (color)") { # color
    res <- color(score = score,
                 age = age,
                 education_years = education_years)
    
    
  } 
  if(test == "Stroop color-word interference test (interference)") { # wordcolor
    res <- wordcolor(score = score,
                     age = age,
                     education_years = education_years)
    
    
  } 
  if(test == "Tower of London - total correct score") { # TOL CS
    res <- TOL_CS(score = score,
                  age = age,
                  education_years = education_years)
    
    
  } 
  if(test == "Tower of London - total execution time") { # TOL ET
    res <- TOL_ET(score = score,
                  age = age,
                  education_years = education_years)
    
    
  } 
  if(test == "Tower of London - total initation time") { # TOL IT
    res <- TOL_IT(score = score,
                  age = age,
                  education_years = education_years)
    
    
  } 
  if(test == "Tower of London - total move score") { # TOL MS
    res <- TOL_MS(score = score,
                  age = age,
                  education_years = education_years)
    
    
  } 
  if(test == "Tower of London - total problem-solving time score") { # TOL PT
    res <- TOL_PT(score = score,
                  age = age,
                  education_years = education_years)
    
    
  } 
  if(test == "Rey-Osterrieth complex figure copy raw") { # ROCF_C_RAW
    res <- ROCF_C_RAW(score = score,
                      age = age,
                      education_years = education_years)
    
    
  } 
  if(test == "Rey-Osterrieth complex figure copy time") { # ROCF_C_TIME
    res <- ROCF_C_TIME(score = score,
                       age = age,
                       education_years = education_years)
    
    
  } 
  if(test == "Rey-Osterrieth complex figure memory delayed recall") { # ROCF_M_DR
    res <- ROCF_M_DR(score = score,
                     age = age,
                     education_years = education_years)
    
    
  } 
  if(test == "Rey-Osterrieth complex figure memory immediate recall") { # ROCF_M_IR
    res <- ROCF_M_IR(score = score,
                     age = age,
                     education_years = education_years)
    
    
  } 
  if(test == "Free and cued selective reminding test delayed total recall") { # FCSRT_DTR
    res <- FCSRT_DTR(score = score,
                     age = age,
                     education_years = education_years)
    
    
  } 
  if(test == "Free and cued selective reminding test delayed free recall") { # FCSRT_DFR
    res <- FCSRT_DFR(score = score,
                     age = age,
                     education_years = education_years)
    
    
  } 
  if(test == "Free and cued selective reminding test trial 1") { # FCSRT_TRIAL1
    res <- FCSRT_TRIAL1(score = score,
                        age = age,
                        education_years = education_years)
    
    
  } 
  if(test == "Free and cued selective reminding test total recall") { # FCSRT_TR
    res <- FCSRT_TR(score = score,
                    age = age,
                    education_years = education_years)
    
    
  } 
  if(test == "Free and cued selective reminding test total free recall") { # FCSRT_TFR
    res <- FCSRT_TFR(score = score,
                     age = age,
                     education_years = education_years)
    
    
  } 
  if(test == "Free and cued selective reminding test total delayed recall / trial 3 total recall") { # FCSRT_TDR_T3TR
    res <- FCSRT_TDRT3TR(score = score,
                         age = age,
                         education_years = education_years)
    
    
  } 
  if(test == "COWAT_a") { # COWAT_a
    res <- COWAT_a(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "COWAT_animal") { # COWAT_animal
    res <- COWAT_animal(score = score,
                        age = age,
                        education_years = education_years)
    
    
  } 
  if(test == "COWAT_e") { # COWAT_e
    res <- COWAT_e(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "COWAT_fruit") { # COWAT_fruit
    res <- COWAT_fruit(score = score,
                       age = age,
                       education_years = education_years)
    
    
  } 
  if(test == "COWAT_kitchen") { # COWAT_kitchen
    res <- COWAT_kitchen(score = score,
                         age = age,
                         education_years = education_years)
    
    
  } 
  if(test == "COWAT_m") { # # COWAT_m
    res <- COWAT_m(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "COWAT_p") { # COWAT_p
    res <- COWAT_p(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "COWAT_r") { # COWAT_r
    res <- COWAT_r(score = score,
                   age = age,
                   education_years = education_years)
    
    
  } 
  if(test == "COWAT_s") { # COWAT_s
    res <- COWAT_s(score = score,
                   age = age,
                   education_years = education_years)
    
  }
  if(test == "mini mental") { # MMS
    res <- mini_mental_state(score = score,
                             age = age,
                             education_years = education_years)
    
  }
  
  return(res)
}

