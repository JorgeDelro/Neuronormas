# Prueba

library(readxl)


bd <- read_xls("bd/COG_BRUTO.xls")


prueba <- neuronorma(score = bd$Q_COG_TMT_A_PRE,
           age = bd$AGE_PRE,
           education_years = bd$EDUCATIONAL_LEVEL,
           tests = "Trail making test a")


tests = c("Trail making test a", "Trail making test b")




neuronorma <- function(score, age, education_years, tests){
  
  # Dataframe with data
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  # Sanity Check
  for (i in 1:nrow(db)) {
    if(db[i, "education_years"] > 20) { 
      db[i, "education_years"] = 20 }
  }
 
  # Temporary data frame
  res_new <- data.frame(temp = nrow(db))
  
  
  for (i in 1:length(tests)) {
    if(tests[i] == "Trail making test a") { # TMTA
        res <- tmta(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
        
        res_new <- cbind(res_new, res)
    } else if(tests[i] == "Trail making test b") { # TMTB
      res <- tmtb(score = db$score,
                  age = db$age,
                  education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Symbol digit modalities test") { # SMDT
      res <- SDMT(score = db$score,
                  age = db$age,
                  education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Letter-number sequencing raw score") { #LNS_RAW
      res <- LNS_RAW(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Letter-number sequencing last item score") { # LNS_LI
      res <- LNS_LI(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Digit span forward") { # DS_FORWARD
      res <- ds_forward(score = db$score,
                        age = db$age,
                        education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Digit span backward") { # DS_BACKWARD
      res <- ds_backward(score = db$score,
                         age = db$age,
                         education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Coris blocks forward raw score") { # CB_F_RAW
      res <- CB_F_RAW(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Coris blocks forward last item score") { # CB_F_LI
      res <- CB_F_LI(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Coris blocks backward raw score") { # CB_B_RAW
      res <- CB_B_RAW(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Coris blocks backward last item score") { # CB_B_LI
      res <- CB_B_LI(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Stroop color-word interference test (word)") { # word
      res <- word(score = db$score,
                  age = db$age,
                  education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Stroop color-word interference test (color)") { # color
      res <- color(score = db$score,
                   age = db$age,
                   education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Stroop color-word interference test (interference)") { # wordcolor
      res <- wordcolor(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Tower of London - total correct score") { # TOL CS
      res <- TOL_CS(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Tower of London - total execution time") { # TOL ET
      res <- TOL_ET(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Tower of London - total initation time") { # TOL IT
      res <- TOL_IT(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Tower of London - total move score") { # TOL MS
      res <- TOL_MS(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Tower of London - total problem-solving time score") { # TOL PT
      res <- TOL_PT(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Rey-Osterrieth complex figure copy raw") { # ROCF_C_RAW
      res <- ROCF_C_RAW(score = db$score,
                        age = db$age,
                        education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Rey-Osterrieth complex figure copy time") { # ROCF_C_TIME
      res <- ROCF_C_TIME(score = db$score,
                         age = db$age,
                         education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Rey-Osterrieth complex figure memory delayed recall") { # ROCF_M_DR
      res <- ROCF_M_DR(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Rey-Osterrieth complex figure memory immediate recall") { # ROCF_M_IR
      res <- ROCF_M_IR(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Free and cued selective reminding test delayed total recall") { # FCSRT_DTR
      res <- FCSRT_DTR(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Free and cued selective reminding test delayed free recall") { # FCSRT_DFR
      res <- FCSRT_DFR(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Free and cued selective reminding test trial 1") { # FCSRT_TRIAL1
      res <- FCSRT_TRIAL1(score = db$score,
                          age = db$age,
                          education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Free and cued selective reminding test total recall") { # FCSRT_TR
      res <- FCSRT_TR(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Free and cued selective reminding test total free recall") { # FCSRT_TFR
      res <- FCSRT_TFR(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Free and cued selective reminding test total delayed recall / trial 3 total recall") { # FCSRT_TDR_T3TR
      res <- FCSRT_TDRT3TR(score = db$score,
                           age = db$age,
                           education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_a") { # COWAT_a
      res <- COWAT_a(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_animal") { # COWAT_animal
      res <- COWAT_animal(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_e") { # COWAT_e
      res <- COWAT_e(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_fruit") { # COWAT_fruit
      res <- COWAT_fruit(score = db$score,
                   age = db$age,
                   education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_kitchen") { # COWAT_kitchen
      res <- COWAT_kitchen(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_m") { # # COWAT_m
      res <- COWAT_m(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_p") { # COWAT_p
      res <- COWAT_p(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "COWAT_r") { # COWAT_r
      res <- COWAT_r(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "s") { # COWAT_s
      res <- s(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    }
    
    }
  
  res_new <- cbind(db, res_new[-1])
        
  return(res_new)
}

neuronorma <- function(score, age, education_years, tests){
  
  # Dataframe with data
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  # Sanity Check
  for (i in 1:nrow(db)) {
    if(db[i, "education_years"] > 10) { 
      db[i, "education_years"] <- 10 }
  }
  
  if(is.na(tests)) {
    print("Please select a test of the following lists: \n
          - trail_making_test_a  \n
          - trail_making_test_b")
  } else {
    
    #  
    res_new <- data.frame()
    
    for (i in tests) {
      if(tests == "Trail making test a") {
        res <- tmta(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
        
        res_new <- cbind(res_new, res)
        
      } else if(tests == "Trail making test b") {
        res <- tmtb(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
        
        res_new <- cbind(res_new, res)
        
      }
      
    }}
  
  return(res)
  }
