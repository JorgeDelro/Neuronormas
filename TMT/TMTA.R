# trail making test A 
#
# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 

tmta <- function(score, age, education_years){
  
  tmta_db <- data.frame(score = score, age = age, education_years = education_years)
  tmta_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(tmta_db)) {
    res <- tmta_scale_score(score = tmta_db[i, "score"], 
                              age = tmta_db[i, "age"],
                              education_years = tmta_db[i, "education_years"])
    tmta_new <- rbind(tmta_new, res)
  }
  
  
  return(tmta_new[,c("tmta_scale_score", "tmta_percentil_range", "tmta_NSSae")])
}

tmta_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  

  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 17, 18, ifelse (
        db$score <= 22, 17, ifelse (
          db$score <= 23, 16, ifelse (
            db$score <= 24, 15, ifelse (
              db$score <= 25, 14, ifelse (
                db$score <= 28, 13, ifelse (
                  db$score <= 33, 12, ifelse (
                    db$score <= 35, 11, ifelse (
                      db$score <= 46, 10, ifelse (
                        db$score <= 53, 9, ifelse (
                          db$score <= 58, 8, ifelse (
                            db$score <= 67, 7, ifelse (
                              db$score <= 80, 6, ifelse (
                                db$score <= 101, 5, ifelse (
                                  db$score <= 117, 4, ifelse (
                                    db$score <= 120, 3, ifelse (
                                      db$score >= 121, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmta_percentil_range <- with (db, ifelse (
      db$score <= 17, "> 99" , ifelse (
        db$score <= 22, "99" , ifelse (
          db$score <= 23, "98" , ifelse (
            db$score <= 24, "95-97" , ifelse (
              db$score <= 25, "90-94" , ifelse (
                db$score <= 28, "82-89" , ifelse (
                  db$score <= 33, "72-81" , ifelse (
                    db$score <= 35, "60-71" , ifelse (
                      db$score <= 46, "41-59" , ifelse (
                        db$score <= 53, "29-40" , ifelse (
                          db$score <= 58, "19-28" , ifelse (
                            db$score <= 67, "11-18" , ifelse (
                              db$score <= 80, "6-10" , ifelse (
                                db$score <= 101, "3-5" , ifelse (
                                  db$score <= 117, "2" , ifelse (
                                    db$score <= 120, "1" , ifelse (
                                      db$score >= 121, "<1" , NA ))))))))))))))))))
    
    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 17, 18, ifelse (
        db$score <= 22, 17, ifelse (
          db$score <= 23, 16, ifelse (
            db$score <= 24, 15, ifelse (
              db$score <= 25, 14, ifelse (
                db$score <= 29, 13, ifelse (
                  db$score <= 34, 12, ifelse (
                    db$score <= 40, 11, ifelse (
                      db$score <= 49, 10, ifelse (
                        db$score <= 55, 9, ifelse (
                          db$score <= 60, 8, ifelse (
                            db$score <= 72, 7, ifelse (
                              db$score <= 86, 6, ifelse (
                                db$score <= 109, 5, ifelse (
                                  db$score <= 117, 4, ifelse (
                                    db$score <= 120, 3, ifelse (
                                      db$score >= 121, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmta_percentil_range <- with (db, ifelse (
      db$score <= 17, "> 99" , ifelse (
        db$score <= 22, "99" , ifelse (
          db$score <= 23, "98" , ifelse (
            db$score <= 24, "95-97" , ifelse (
              db$score <= 25, "90-94" , ifelse (
                db$score <= 29, "82-89" , ifelse (
                  db$score <= 34, "72-81" , ifelse (
                    db$score <= 40, "60-71" , ifelse (
                      db$score <= 49, "41-59" , ifelse (
                        db$score <= 55, "29-40" , ifelse (
                          db$score <= 60, "19-28" , ifelse (
                            db$score <= 72, "11-18" , ifelse (
                              db$score <= 86, "6-10" , ifelse (
                                db$score <= 109, "3-5" , ifelse (
                                  db$score <= 117, "2" , ifelse (
                                    db$score <= 120, "1" , ifelse (
                                      db$score >= 121, "<1" , NA ))))))))))))))))))
    
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 23, 18, ifelse (
        
        db$score <= 24, 16, ifelse (
          db$score <= 25, 15, ifelse (
            db$score <= 26, 14, ifelse (
              db$score <= 31, 13, ifelse (
                db$score <= 35, 12, ifelse (
                  db$score <= 41, 11, ifelse (
                    db$score <= 54, 10, ifelse (
                      db$score <= 59, 9, ifelse (
                        db$score <= 67, 8, ifelse (
                          db$score <= 84, 7, ifelse (
                            db$score <= 102, 6, ifelse (
                              db$score <= 119, 5, ifelse (
                                
                                db$score <= 130, 3, ifelse (
                                  db$score >= 131, 2, NA ))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 23, "> 99" , ifelse (
    
    db$score <= 24, "98" , ifelse (
      db$score <= 25, "95-97" , ifelse (
        db$score <= 26, "90-94" , ifelse (
          db$score <= 31, "82-89" , ifelse (
            db$score <= 35, "72-81" , ifelse (
              db$score <= 41, "60-71" , ifelse (
                db$score <= 54, "41-59" , ifelse (
                  db$score <= 59, "29-40" , ifelse (
                    db$score <= 67, "19-28" , ifelse (
                      db$score <= 84, "11-18" , ifelse (
                        db$score <= 102, "6-10" , ifelse (
                          db$score <= 119, "3-5" , ifelse (
                            
                            db$score <= 130, "1" , ifelse (
                              db$score >= 131, "<1" , NA ))))))))))))))))
    
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 23, 18, ifelse (
        
        db$score <= 24, 16, ifelse (
          db$score <= 26, 15, ifelse (
            db$score <= 29, 14, ifelse (
              db$score <= 32, 13, ifelse (
                db$score <= 37, 12, ifelse (
                  db$score <= 46, 11, ifelse (
                    db$score <= 58, 10, ifelse (
                      db$score <= 63, 9, ifelse (
                        db$score <= 74, 8, ifelse (
                          db$score <= 86, 7, ifelse (
                            db$score <= 103, 6, ifelse (
                              db$score <= 111, 5, ifelse (
                                db$score <= 120, 4, ifelse (
                                  db$score <= 131, 3, ifelse (
                                    db$score >= 132, 2, NA )))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 23, "> 99" , ifelse (
    
    db$score <= 24, "98" , ifelse (
      db$score <= 26, "95-97" , ifelse (
        db$score <= 29, "90-94" , ifelse (
          db$score <= 32, "82-89" , ifelse (
            db$score <= 37, "72-81" , ifelse (
              db$score <= 46, "60-71" , ifelse (
                db$score <= 58, "41-59" , ifelse (
                  db$score <= 63, "29-40" , ifelse (
                    db$score <= 74, "19-28" , ifelse (
                      db$score <= 86, "11-18" , ifelse (
                        db$score <= 103, "6-10" , ifelse (
                          db$score <= 111, "3-5" , ifelse (
                            db$score <= 120, "2" , ifelse (
                              db$score <= 131, "1" , ifelse (
                                db$score >= 132, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 23, 18, ifelse (
        
        db$score <= 24, 16, ifelse (
          db$score <= 30, 15, ifelse (
            db$score <= 32, 14, ifelse (
              db$score <= 36, 13, ifelse (
                db$score <= 43, 12, ifelse (
                  db$score <= 48, 11, ifelse (
                    db$score <= 57, 10, ifelse (
                      db$score <= 62, 9, ifelse (
                        db$score <= 73, 8, ifelse (
                          db$score <= 86, 7, ifelse (
                            db$score <= 103, 6, ifelse (
                              db$score <= 124, 5, ifelse (
                                db$score <= 131, 4, ifelse (
                                  db$score <= 137, 3, ifelse (
                                    db$score >= 138, 2, NA )))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 23, "> 99" , ifelse (
    
    db$score <= 24, "98" , ifelse (
      db$score <= 30, "95-97" , ifelse (
        db$score <= 32, "90-94" , ifelse (
          db$score <= 36, "82-89" , ifelse (
            db$score <= 43, "72-81" , ifelse (
              db$score <= 48, "60-71" , ifelse (
                db$score <= 57, "41-59" , ifelse (
                  db$score <= 62, "29-40" , ifelse (
                    db$score <= 73, "19-28" , ifelse (
                      db$score <= 86, "11-18" , ifelse (
                        db$score <= 103, "6-10" , ifelse (
                          db$score <= 124, "3-5" , ifelse (
                            db$score <= 131, "2" , ifelse (
                              db$score <= 137, "1" , ifelse (
                                db$score >= 138, "<1" , NA ))))))))))))))))) 
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 24, 18, ifelse (
        
        db$score <= 27, 16, ifelse (
          db$score <= 31, 15, ifelse (
            db$score <= 33, 14, ifelse (
              db$score <= 37, 13, ifelse (
                db$score <= 44, 12, ifelse (
                  db$score <= 50, 11, ifelse (
                    db$score <= 59, 10, ifelse (
                      db$score <= 67, 9, ifelse (
                        db$score <= 78, 8, ifelse (
                          db$score <= 109, 7, ifelse (
                            db$score <= 120, 6, ifelse (
                              db$score <= 157, 5, ifelse (
                                db$score <= 158, 4, ifelse (
                                  db$score <= 170, 3, ifelse (
                                    db$score >= 171, 2, NA )))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 24, "> 99" , ifelse (
    
    db$score <= 27, "98" , ifelse (
      db$score <= 31, "95-97" , ifelse (
        db$score <= 33, "90-94" , ifelse (
          db$score <= 37, "82-89" , ifelse (
            db$score <= 44, "72-81" , ifelse (
              db$score <= 50, "60-71" , ifelse (
                db$score <= 59, "41-59" , ifelse (
                  db$score <= 67, "29-40" , ifelse (
                    db$score <= 78, "19-28" , ifelse (
                      db$score <= 109, "11-18" , ifelse (
                        db$score <= 120, "6-10" , ifelse (
                          db$score <= 157, "3-5" , ifelse (
                            db$score <= 158, "2" , ifelse (
                              db$score <= 170, "1" , ifelse (
                                db$score >= 171, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 28, 18, ifelse (
        db$score <= 29, 17, ifelse (
          db$score <= 30, 16, ifelse (
            db$score <= 33, 15, ifelse (
              db$score <= 35, 14, ifelse (
                db$score <= 42, 13, ifelse (
                  db$score <= 46, 12, ifelse (
                    db$score <= 53, 11, ifelse (
                      db$score <= 63, 10, ifelse (
                        db$score <= 72, 9, ifelse (
                          db$score <= 84, 8, ifelse (
                            db$score <= 109, 7, ifelse (
                              db$score <= 137, 6, ifelse (
                                db$score <= 159, 5, ifelse (
                                  db$score <= 160, 4, ifelse (
                                    db$score <= 170, 3, ifelse (
                                      db$score >= 171, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmta_percentil_range <- with (db, ifelse (
      db$score <= 28, "> 99" , ifelse (
        db$score <= 29, "99" , ifelse (
          db$score <= 30, "98" , ifelse (
            db$score <= 33, "95-97" , ifelse (
              db$score <= 35, "90-94" , ifelse (
                db$score <= 42, "82-89" , ifelse (
                  db$score <= 46, "72-81" , ifelse (
                    db$score <= 53, "60-71" , ifelse (
                      db$score <= 63, "41-59" , ifelse (
                        db$score <= 72, "29-40" , ifelse (
                          db$score <= 84, "19-28" , ifelse (
                            db$score <= 109, "11-18" , ifelse (
                              db$score <= 137, "6-10" , ifelse (
                                db$score <= 159, "3-5" , ifelse (
                                  db$score <= 160, "2" , ifelse (
                                    db$score <= 170, "1" , ifelse (
                                      db$score >= 171, "<1" , NA )))))))))))))))))) 
    
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 29, 18, ifelse (
        db$score <= 33, 17, ifelse (
          
          db$score <= 35, 15, ifelse (
            db$score <= 37, 14, ifelse (
              db$score <= 44, 13, ifelse (
                db$score <= 47, 12, ifelse (
                  db$score <= 56, 11, ifelse (
                    db$score <= 67, 10, ifelse (
                      db$score <= 77, 9, ifelse (
                        db$score <= 87, 8, ifelse (
                          db$score <= 109, 7, ifelse (
                            db$score <= 137, 6, ifelse (
                              db$score <= 159, 5, ifelse (
                                db$score <= 160, 4, ifelse (
                                  db$score <= 170, 3, ifelse (
                                    db$score >= 171, 2, NA )))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 29, "> 99" , ifelse (
    db$score <= 33, "99" , ifelse (
      
      db$score <= 35, "95-97" , ifelse (
        db$score <= 37, "90-94" , ifelse (
          db$score <= 44, "82-89" , ifelse (
            db$score <= 47, "72-81" , ifelse (
              db$score <= 56, "60-71" , ifelse (
                db$score <= 67, "41-59" , ifelse (
                  db$score <= 77, "29-40" , ifelse (
                    db$score <= 87, "19-28" , ifelse (
                      db$score <= 109, "11-18" , ifelse (
                        db$score <= 137, "6-10" , ifelse (
                          db$score <= 159, "3-5" , ifelse (
                            db$score <= 160, "2" , ifelse (
                              db$score <= 170, "1" , ifelse (
                                db$score >= 171, "<1" , NA )))))))))))))))))  
    
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 29, 18, ifelse (
        db$score <= 30, 17, ifelse (
          
          db$score <= 35, 15, ifelse (
            db$score <= 41, 14, ifelse (
              db$score <= 42, 13, ifelse (
                db$score <= 54, 12, ifelse (
                  db$score <= 62, 11, ifelse (
                    db$score <= 73, 10, ifelse (
                      db$score <= 79, 9, ifelse (
                        db$score <= 87, 8, ifelse (
                          db$score <= 105, 7, ifelse (
                            db$score <= 145, 6, ifelse (
                              db$score <= 160, 5, ifelse (
                                db$score <= 169, 4, ifelse (
                                  db$score <= 170, 3, ifelse (
                                    db$score >= 171, 2, NA )))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 29, "> 99" , ifelse (
    db$score <= 30, "99" , ifelse (
      
      db$score <= 35, "95-97" , ifelse (
        db$score <= 41, "90-94" , ifelse (
          db$score <= 42, "82-89" , ifelse (
            db$score <= 54, "72-81" , ifelse (
              db$score <= 62, "60-71" , ifelse (
                db$score <= 73, "41-59" , ifelse (
                  db$score <= 79, "29-40" , ifelse (
                    db$score <= 87, "19-28" , ifelse (
                      db$score <= 105, "11-18" , ifelse (
                        db$score <= 145, "6-10" , ifelse (
                          db$score <= 160, "3-5" , ifelse (
                            db$score <= 169, "2" , ifelse (
                              db$score <= 170, "1" , ifelse (
                                db$score >= 171, "<1" , NA ))))))))))))))))) 
    
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$tmta_scale_score <- with ( db, ifelse (
      db$score <= 23, 18, ifelse (
        
        db$score <= 24, 16, ifelse (
          db$score <= 36, 15, ifelse (
            db$score <= 41, 14, ifelse (
              db$score <= 42, 13, ifelse (
                db$score <= 57, 12, ifelse (
                  db$score <= 64, 11, ifelse (
                    db$score <= 74, 10, ifelse (
                      db$score <= 80, 9, ifelse (
                        db$score <= 87, 8, ifelse (
                          db$score <= 91, 7, ifelse (
                            db$score <= 105, 6, ifelse (
                              db$score <= 159, 5, ifelse (
                                db$score <= 160, 4, ifelse (
                                  
                                  db$score >= 161, 2, NA ))))))))))))))))

# percentile score

db$tmta_percentil_range <- with (db, ifelse (
  db$score <= 23, "> 99" , ifelse (
    
    db$score <= 24, "98" , ifelse (
      db$score <= 36, "95-97" , ifelse (
        db$score <= 41, "90-94" , ifelse (
          db$score <= 42, "82-89" , ifelse (
            db$score <= 57, "72-81" , ifelse (
              db$score <= 64, "60-71" , ifelse (
                db$score <= 74, "41-59" , ifelse (
                  db$score <= 80, "29-40" , ifelse (
                    db$score <= 87, "19-28" , ifelse (
                      db$score <= 91, "11-18" , ifelse (
                        db$score <= 105, "6-10" , ifelse (
                          db$score <= 159, "3-5" , ifelse (
                            db$score <= 160, "2" , ifelse (
                              
                              db$score >= 161, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$tmta_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 3, db$tmta_scale_score + 1, ifelse(
      db$education_years >= 4  & db$education_years <= 8, db$tmta_scale_score, ifelse(
        db$education_years >= 9  & db$education_years <= 12, db$tmta_scale_score - 1, ifelse(
          db$education_years >= 13  & db$education_years <= 17, db$tmta_scale_score - 2, ifelse(
            db$education_years >= 18  & db$education_years <= 20, db$tmta_scale_score- 3, ifelse(
            )))))))
  
  
  # NSSae
  db$tmta_NSSae <- db$tmta_scale_score - (-0.21832*(db$tmta_education_years_adj-12)) 
  
  return(db)
}
