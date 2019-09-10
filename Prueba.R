# Prueba

library(readxl)


bd <- read_xls("bd/cognitivo_EFICCOM.xls")

source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/TMTA.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/TMTB.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/SDMT.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/LNS_RAW.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/LNS_LI.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/DS_FORWARD.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/DS_BACKWARD.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/CB_F_RAW.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/CB_F_LI.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/CB_B_RAW.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/TMT/CB_B_LI.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/WORD.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/COLOR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/WORDCOLOR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/TOL CS.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/TOL ET.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/TOL IT.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/TOL MS.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/STROOP/TOL PT.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/ROCF_C_RAW.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/ROCF_C_TIME.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/ROCF_M_DR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/ROCF_M_IR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/FCSRT_DTR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/FCSRT_DFR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/FCSRT_TRIAL1.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/FCSRT_TR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/FCSRT_TFR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/REY_OSTERRIETH/FCSRT_TDR_T3TR.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/A.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/E.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/fruit.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/kitchen.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/M.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/P.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/R.R")
source("/Users/jorge/Documents/Curso_2020_2021/Proyectos/Neuronormas/COWAT/S.R")


prueba <- neuronorma(score = bd$Q_COG_TMT_A_PRE,
           age = bd$AGE_PRE,
           education_years = bd$EDUCATIONAL_LEVEL,
           tests = c("Trail making test a", 
                     "Trail making test b",
                     "Symbol digit modalities test",
                     "Letter-number sequencing raw score",
                     "Letter-number sequencing last item score",
                     "Digit span forward",
                     "Digit span backward",
                     "Coris blocks forward raw score",
                     "Coris blocks forward last item score",
                     "Coris blocks backward raw score",
                     "Coris blocks backward last item score",
                     "Stroop color-word interference test (word)",
                     "Stroop color-word interference test (color)",
                     "Stroop color-word interference test (interference)",
                     "Tower of London - total correct score",
                     "Tower of London - total execution time",
                     "Tower of London - total initation time",
                     "Tower of London - total move score",
                     "Tower of London - total problem-solving time score",
                     "Rey-Osterrieth complex figure copy raw",
                     "Rey-Osterrieth complex figure copy time",
                     "Rey-Osterrieth complex figure memory delayed recall",
                     "Rey-Osterrieth complex figure memory immediate recall",
                     "Free and cued selective reminding test delayed total recall",
                     "Free and cued selective reminding test delayed free recall",
                     "Free and cued selective reminding test trial 1",
                     "Free and cued selective reminding test total recall",
                     "Free and cued selective reminding test total free recall",
                     "Free and cued selective reminding test total delayed recall / trial 3 total recall",
                     "COWAT_a",
                     "COWAT_animal",
                     "COWAT_e",
                     "COWAT_fruit",
                     "COWAT_kitchen",
                     "COWAT_m",
                     "COWAT_p",
                     "COWAT_r",
                     "COWAT_s"))


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
      
    } else if(test == "Symbol digit modalities test") { # SDMT
      res <- SDMT(score = db$score,
                  age = db$age,
                  education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    } else if(test == "Letter-number sequencing raw score") { # LNS_RAW
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
      
    } else if(test == "COWAT_s") { # COWAT_s
      res <- COWAT_s(score = db$score,
               age = db$age,
               education_years = db$education_years)
      
      res_new <- cbind(res_new, res)
      
    }
    
    }
  
  res_new <- cbind(db, res_new[-1])
        
  return(res_new)
}

### Results




