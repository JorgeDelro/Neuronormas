# Neuronormas

neuronorma <- function(score, age, education_years, tests){
  
  # Dataframe with data
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  # Sanity Check
  if(is.na(tests)) {
    print("Please select a test of the following lists: \n
          - trail_making_test_a  \n
          - trail_making_test_b")
  } else {
    
    #  
    res_new <- data.frame()
    
    for (i in tests) {
      if(test == "Trail making_test a") {
        res <- tmta(score = db$score,
                                   age = db$age,
                                   education_years = db$education_years)
      } else if(test == "Trail making_test b") {
        res <- tmtb(score = db$score,
                                   age = db$age,
                                   education_years = db$education_years)
      } else if(test == "Symbol digit modalities test") {
        res <- SDMT(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      } else if(test == "Letter-number sequencing raw score") {
        res <- LNS_RAW(score = db$score,
                    age = db$age,
                    education_years = db$education_years)
      } else if(test == "Letter-number sequencing last item score") {
        res <- LNS_LI(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      } else if(test == "Digit span forward") {
        res <- ds_forward(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      } else if(test == "Digit span backward") {
        res <- ds_backward(score = db$score,
                          age = db$age,
                          education_years = db$education_years)
      } else if(test == "Coris blocks forward raw score") {
        res <- CB_F_RAW(score = db$score,
                           age = db$age,
                           education_years = db$education_years)
      } else if(test == "Coris blocks forward last item score") {
        res <- CB_F_LI(score = db$score,
                        age = db$age,
                        education_years = db$education_years)
      } else if(test == "Coris blocks backward raw score") {
        res <- CB_B_RAW(score = db$score,
                        age = db$age,
                        education_years = db$education_years)
      } else if(test == "Coris blocks backward last item score") {
        res <- CB_B_LI(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      } else if(test == "Stroop color-word interference test (word)") {
        res <- word(score = db$score,
                       age = db$age,
                       education_years = db$education_years)
      } else if(test == "Stroop color-word interference test (color)") {
        res <- color(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      } else if(test == "Stroop color-word interference test (interference)") {
        res <- wordcolor(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      } else if(test == "Tower of London - total correct score") {
        res <- TOL_CS(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      } else if(test == "Tower of London - total execution time") {
        res <- TOL_ET(score = db$score,
                     age = db$age,
                     education_years = db$education_years)
      } else if(test == "Tower of London - total initation time") {
        res <- TOL_IT(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      } else if(test == "Tower of London - total move score") {
        res <- TOL_MS(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      } else if(test == "Tower of London - total problem-solving time score") {
        res <- TOL_PT(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      } else if(test == "Rey-Osterrieth complex figure copy raw") {
        res <- TOL_PT(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      } else if(test == "Rey-Osterrieth complex figure copy time") {
        res <- TOL_PT(score = db$score,
                      age = db$age,
                      education_years = db$education_years)
      }
      
      res_new <- cbind(res_new, res)
    }
    
  }
  
  
  
  return(res)
}