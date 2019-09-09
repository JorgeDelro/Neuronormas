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
