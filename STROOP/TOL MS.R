# Tower of London - total move score

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

# 
TOL_MS <- function(score, age, education_years){
  
  TOL_MS_db <- data.frame(score = score, age = age, education_years = education_years)
  TOL_MS_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(TOL_MS_db)) {
    res <- TOL_MS_scale_score(score = TOL_MS_db[i, "score"], 
                              age = TOL_MS_db[i, "age"],
                              education_years = TOL_MS_db[i, "education_years"])
    TOL_MS_new <- rbind(TOL_MS_new, res)
  }
  
  return(TOL_MS_new[,c("TOL_MS_scale_score", "TOL_MS_percentil_range", "TOL_MS_NSSae")])
}

TOL_MS_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 2, 18, ifelse (
        db$score <= 3, 17, ifelse (
          
          db$score <= 5, 15, ifelse (
            db$score <= 10, 14, ifelse (
              db$score <= 13, 13, ifelse (
                db$score <= 18, 12, ifelse (
                  db$score <= 23, 11, ifelse (
                    db$score <= 33, 10, ifelse (
                      db$score <= 41, 9, ifelse (
                        db$score <= 47, 8, ifelse (
                          db$score <= 53, 7, ifelse (
                            db$score <= 62, 6, ifelse (
                              db$score <= 74, 5, ifelse (
                                db$score <= 82, 4, ifelse (
                                  db$score <= 92, 3, ifelse (
                                    db$score >= 93, 2, NA )))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 2, "> 99" , ifelse (
    db$score <= 3, "99" , ifelse (
      
      db$score <= 5, "95-97" , ifelse (
        db$score <= 10, "90-94" , ifelse (
          db$score <= 13, "82-89" , ifelse (
            db$score <= 18, "72-81" , ifelse (
              db$score <= 23, "60-71" , ifelse (
                db$score <= 33, "41-59" , ifelse (
                  db$score <= 41, "29-40" , ifelse (
                    db$score <= 47, "19-28" , ifelse (
                      db$score <= 53, "11-18" , ifelse (
                        db$score <= 62, "6-10" , ifelse (
                          db$score <= 74, "3-5" , ifelse (
                            db$score <= 82, "2" , ifelse (
                              db$score <= 92, "1" , ifelse (
                                db$score >= 93, "<1" , NA )))))))))))))))))

    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 3, 18, ifelse (
        db$score <= 4, 17, ifelse (
          db$score <= 5, 16, ifelse (
            db$score <= 10, 15, ifelse (
              db$score <= 13, 14, ifelse (
                db$score <= 16, 13, ifelse (
                  db$score <= 21, 12, ifelse (
                    db$score <= 26, 11, ifelse (
                      db$score <= 34, 10, ifelse (
                        db$score <= 40, 9, ifelse (
                          db$score <= 47, 8, ifelse (
                            db$score <= 53, 7, ifelse (
                              db$score <= 62, 6, ifelse (
                                db$score <= 79, 5, ifelse (
                                  db$score <= 82, 4, ifelse (
                                    db$score <= 92, 3, ifelse (
                                      db$score >= 93, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_MS_percentil_range <- with (db, ifelse (
      db$score <= 3, "> 99" , ifelse (
        db$score <= 4, "99" , ifelse (
          db$score <= 5, "98" , ifelse (
            db$score <= 10, "95-97" , ifelse (
              db$score <= 13, "90-94" , ifelse (
                db$score <= 16, "82-89" , ifelse (
                  db$score <= 21, "72-81" , ifelse (
                    db$score <= 26, "60-71" , ifelse (
                      db$score <= 34, "41-59" , ifelse (
                        db$score <= 40, "29-40" , ifelse (
                          db$score <= 47, "19-28" , ifelse (
                            db$score <= 53, "11-18" , ifelse (
                              db$score <= 62, "6-10" , ifelse (
                                db$score <= 79, "3-5" , ifelse (
                                  db$score <= 82, "2" , ifelse (
                                    db$score <= 92, "1" , ifelse (
                                      db$score >= 93, "<1" , NA ))))))))))))))))))
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 4, 18, ifelse (
        db$score <= 5, 17, ifelse (
          db$score <= 8, 16, ifelse (
            db$score <= 10, 15, ifelse (
              db$score <= 13, 14, ifelse (
                db$score <= 17, 13, ifelse (
                  db$score <= 21, 12, ifelse (
                    db$score <= 26, 11, ifelse (
                      db$score <= 35, 10, ifelse (
                        db$score <= 41, 9, ifelse (
                          db$score <= 48, 8, ifelse (
                            db$score <= 56, 7, ifelse (
                              db$score <= 63, 6, ifelse (
                                db$score <= 79, 5, ifelse (
                                  db$score <= 82, 4, ifelse (
                                    db$score <= 92, 3, ifelse (
                                      db$score >= 93, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_MS_percentil_range <- with (db, ifelse (
      db$score <= 4, "> 99" , ifelse (
        db$score <= 5, "99" , ifelse (
          db$score <= 8, "98" , ifelse (
            db$score <= 10, "95-97" , ifelse (
              db$score <= 13, "90-94" , ifelse (
                db$score <= 17, "82-89" , ifelse (
                  db$score <= 21, "72-81" , ifelse (
                    db$score <= 26, "60-71" , ifelse (
                      db$score <= 35, "41-59" , ifelse (
                        db$score <= 41, "29-40" , ifelse (
                          db$score <= 48, "19-28" , ifelse (
                            db$score <= 56, "11-18" , ifelse (
                              db$score <= 63, "6-10" , ifelse (
                                db$score <= 79, "3-5" , ifelse (
                                  db$score <= 82, "2" , ifelse (
                                    db$score <= 92, "1" , ifelse (
                                      db$score >= 93, "<1" , NA ))))))))))))))))))
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 4, 18, ifelse (
        db$score <= 5, 17, ifelse (
          db$score <= 6, 16, ifelse (
            db$score <= 10, 15, ifelse (
              db$score <= 13, 14, ifelse (
                db$score <= 18, 13, ifelse (
                  db$score <= 23, 12, ifelse (
                    db$score <= 27, 11, ifelse (
                      db$score <= 36, 10, ifelse (
                        db$score <= 45, 9, ifelse (
                          db$score <= 49, 8, ifelse (
                            db$score <= 60, 7, ifelse (
                              db$score <= 62, 6, ifelse (
                                db$score <= 72, 5, ifelse (
                                  db$score <= 92, 4, ifelse (
                                    db$score <= 103, 3, ifelse (
                                      db$score >= 104, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$TOL_MS_percentil_range <- with (db, ifelse (
      db$score <= 4, "> 99" , ifelse (
        db$score <= 5, "99" , ifelse (
          db$score <= 6, "98" , ifelse (
            db$score <= 10, "95-97" , ifelse (
              db$score <= 13, "90-94" , ifelse (
                db$score <= 18, "82-89" , ifelse (
                  db$score <= 23, "72-81" , ifelse (
                    db$score <= 27, "60-71" , ifelse (
                      db$score <= 36, "41-59" , ifelse (
                        db$score <= 45, "29-40" , ifelse (
                          db$score <= 49, "19-28" , ifelse (
                            db$score <= 60, "11-18" , ifelse (
                              db$score <= 62, "6-10" , ifelse (
                                db$score <= 72, "3-5" , ifelse (
                                  db$score <= 92, "2" , ifelse (
                                    db$score <= 103, "1" , ifelse (
                                      db$score >= 104, "<1" , NA ))))))))))))))))))
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 5, 18, ifelse (
        db$score <= 6, 17, ifelse (
          
          db$score <= 9, 15, ifelse (
            db$score <= 13, 14, ifelse (
              db$score <= 16, 13, ifelse (
                db$score <= 20, 12, ifelse (
                  db$score <= 26, 11, ifelse (
                    db$score <= 35, 10, ifelse (
                      db$score <= 41, 9, ifelse (
                        db$score <= 48, 8, ifelse (
                          db$score <= 56, 7, ifelse (
                            db$score <= 62, 6, ifelse (
                              db$score <= 72, 5, ifelse (
                                db$score <= 73, 4, ifelse (
                                  db$score <= 103, 3, ifelse (
                                    db$score >= 104, 2, NA )))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 5, "> 99" , ifelse (
    db$score <= 6, "99" , ifelse (
      
      db$score <= 9, "95-97" , ifelse (
        db$score <= 13, "90-94" , ifelse (
          db$score <= 16, "82-89" , ifelse (
            db$score <= 20, "72-81" , ifelse (
              db$score <= 26, "60-71" , ifelse (
                db$score <= 35, "41-59" , ifelse (
                  db$score <= 41, "29-40" , ifelse (
                    db$score <= 48, "19-28" , ifelse (
                      db$score <= 56, "11-18" , ifelse (
                        db$score <= 62, "6-10" , ifelse (
                          db$score <= 72, "3-5" , ifelse (
                            db$score <= 73, "2" , ifelse (
                              db$score <= 103, "1" , ifelse (
                                db$score >= 104, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 5, 18, ifelse (
        db$score <= 6, 17, ifelse (
          
          db$score <= 9, 15, ifelse (
            db$score <= 13, 14, ifelse (
              db$score <= 17, 13, ifelse (
                db$score <= 22, 12, ifelse (
                  db$score <= 28, 11, ifelse (
                    db$score <= 37, 10, ifelse (
                      db$score <= 44, 9, ifelse (
                        db$score <= 50, 8, ifelse (
                          db$score <= 60, 7, ifelse (
                            db$score <= 68, 6, ifelse (
                              db$score <= 76, 5, ifelse (
                                db$score <= 92, 4, ifelse (
                                  db$score <= 103, 3, ifelse (
                                    db$score >= 104, 2, NA )))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 5, "> 99" , ifelse (
    db$score <= 6, "99" , ifelse (
      
      db$score <= 9, "95-97" , ifelse (
        db$score <= 13, "90-94" , ifelse (
          db$score <= 17, "82-89" , ifelse (
            db$score <= 22, "72-81" , ifelse (
              db$score <= 28, "60-71" , ifelse (
                db$score <= 37, "41-59" , ifelse (
                  db$score <= 44, "29-40" , ifelse (
                    db$score <= 50, "19-28" , ifelse (
                      db$score <= 60, "11-18" , ifelse (
                        db$score <= 68, "6-10" , ifelse (
                          db$score <= 76, "3-5" , ifelse (
                            db$score <= 92, "2" , ifelse (
                              db$score <= 103, "1" , ifelse (
                                db$score >= 104, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 6, 18, ifelse (
        
        db$score <= 7, 16, ifelse (
          db$score <= 10 , 15, ifelse (
            db$score <= 15 , 14, ifelse (
              db$score <= 19 , 13, ifelse (
                db$score <= 25 , 12, ifelse (
                  db$score <= 31 , 11, ifelse (
                    db$score <= 38 , 10, ifelse (
                      db$score <= 44 , 9, ifelse (
                        db$score <= 50 , 8, ifelse (
                          db$score <= 60 , 7, ifelse (
                            db$score <= 71 , 6, ifelse (
                              db$score <= 78 , 5, ifelse (
                                db$score <= 92 , 4, ifelse (
                                  db$score <= 103, 3, ifelse (
                                    db$score >=  104, 2, NA )))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 6, "> 99" , ifelse (
    
    db$score <= 7, "98" , ifelse (
      db$score <= 10 , "95-97" , ifelse (
        db$score <= 15 , "90-94" , ifelse (
          db$score <= 19 , "82-89" , ifelse (
            db$score <= 25 , "72-81" , ifelse (
              db$score <= 31 , "60-71" , ifelse (
                db$score <= 38 , "41-59" , ifelse (
                  db$score <= 44 , "29-40" , ifelse (
                    db$score <= 50 , "19-28" , ifelse (
                      db$score <= 60 , "11-18" , ifelse (
                        db$score <= 71 , "6-10" , ifelse (
                          db$score <= 78 , "3-5" , ifelse (
                            db$score <= 92 , "2" , ifelse (
                              db$score <= 103, "1" , ifelse (
                                db$score >=  104, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 6, 18, ifelse (
        
        db$score <= 7, 16, ifelse (
          db$score <= 11, 15, ifelse (
            db$score <= 13, 14, ifelse (
              db$score <= 19, 13, ifelse (
                db$score <= 25, 12, ifelse (
                  db$score <= 30, 11, ifelse (
                    db$score <= 39, 10, ifelse (
                      db$score <= 44, 9, ifelse (
                        db$score <= 49, 8, ifelse (
                          db$score <= 58, 7, ifelse (
                            db$score <= 68, 6, ifelse (
                              db$score <= 77, 5, ifelse (
                                db$score <= 78, 4, ifelse (
                                  db$score <= 92, 3, ifelse (
                                    db$score >= 93, 2, NA )))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 6, "> 99" , ifelse (
    
    db$score <= 7, "98" , ifelse (
      db$score <= 11, "95-97" , ifelse (
        db$score <= 13, "90-94" , ifelse (
          db$score <= 19, "82-89" , ifelse (
            db$score <= 25, "72-81" , ifelse (
              db$score <= 30, "60-71" , ifelse (
                db$score <= 39, "41-59" , ifelse (
                  db$score <= 44, "29-40" , ifelse (
                    db$score <= 49, "19-28" , ifelse (
                      db$score <= 58, "11-18" , ifelse (
                        db$score <= 68, "6-10" , ifelse (
                          db$score <= 77, "3-5" , ifelse (
                            db$score <= 78, "2" , ifelse (
                              db$score <= 92, "1" , ifelse (
                                db$score >= 93, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 6, 18, ifelse (
        
        db$score <= 8, 16, ifelse (
          db$score <= 12, 15, ifelse (
            db$score <= 15, 14, ifelse (
              db$score <= 21, 13, ifelse (
                db$score <= 25, 12, ifelse (
                  db$score <= 31, 11, ifelse (
                    db$score <= 39, 10, ifelse (
                      db$score <= 42, 9, ifelse (
                        db$score <= 49, 8, ifelse (
                          db$score <= 60, 7, ifelse (
                            db$score <= 77, 6, ifelse (
                              db$score <= 92, 5, ifelse (
                                
                                db$score <= 93, 3, ifelse (
                                  db$score >= 94, 2, NA ))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 6, "> 99" , ifelse (
    
    db$score <= 8, "98" , ifelse (
      db$score <= 12, "95-97" , ifelse (
        db$score <= 15, "90-94" , ifelse (
          db$score <= 21, "82-89" , ifelse (
            db$score <= 25, "72-81" , ifelse (
              db$score <= 31, "60-71" , ifelse (
                db$score <= 39, "41-59" , ifelse (
                  db$score <= 42, "29-40" , ifelse (
                    db$score <= 49, "19-28" , ifelse (
                      db$score <= 60, "11-18" , ifelse (
                        db$score <= 77, "6-10" , ifelse (
                          db$score <= 92, "3-5" , ifelse (
                            
                            db$score <= 93, "1" , ifelse (
                              db$score >= 94, "<1" , NA ))))))))))))))))
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    
    db$TOL_MS_scale_score <- with ( db, ifelse (
      db$score <= 7, 18, ifelse (
        
        db$score <= 10, 16, ifelse (
          db$score <= 12, 15, ifelse (
            db$score <= 17, 14, ifelse (
              db$score <= 21, 13, ifelse (
                db$score <= 25, 12, ifelse (
                  db$score <= 28, 11, ifelse (
                    db$score <= 39, 10, ifelse (
                      db$score <= 44, 9, ifelse (
                        db$score <= 56, 8, ifelse (
                          db$score <= 76, 7, ifelse (
                            db$score <= 78, 6, ifelse (
                              db$score <= 101, 5, ifelse (
                                db$score <= 102, 4, ifelse (
                                  
                                  db$score >= 103, 2, NA ))))))))))))))))

# percentile score

db$TOL_MS_percentil_range <- with (db, ifelse (
  db$score <= 7, "> 99" , ifelse (
    
    db$score <= 10, "98" , ifelse (
      db$score <= 12, "95-97" , ifelse (
        db$score <= 17, "90-94" , ifelse (
          db$score <= 21, "82-89" , ifelse (
            db$score <= 25, "72-81" , ifelse (
              db$score <= 28, "60-71" , ifelse (
                db$score <= 39, "41-59" , ifelse (
                  db$score <= 44, "29-40" , ifelse (
                    db$score <= 56, "19-28" , ifelse (
                      db$score <= 76, "11-18" , ifelse (
                        db$score <= 78, "6-10" , ifelse (
                          db$score <= 101, "3-5" , ifelse (
                            db$score <= 102, "2" , ifelse (
                              
                              db$score >= 103, "<1" , NA ))))))))))))))))
    
  }
  
  
  # Educational level adjust 
  db$TOL_MS_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 5, db$TOL_MS_scale_score + 1, ifelse(
      db$education_years >= 6  & db$education_years <= 12, db$TOL_MS_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 18, db$TOL_MS_scale_score - 1, ifelse(
          db$education_years >= 19  & db$education_years <= 20, db$TOL_MS_scale_score - 2, ifelse(
            
            ))))))
  
  
  # NSSae
  db$TOL_MS_NSSae <- db$TOL_MS_scale_score - (0.16314*(db$TOL_MS_education_years_adj-12)) 
  
  return(db)
}
