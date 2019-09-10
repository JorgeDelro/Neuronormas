# Stroop color-word interference test (color)

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

color <- function(score, age, education_years){
  
  color_db <- data.frame(score = score, age = age, education_years = education_years)
  color_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(color_db)) {
    res <- color_scale_score(score = color_db[i, "score"], 
                              age = color_db[i, "age"],
                              education_years = color_db[i, "education_years"])
    color_new <- rbind(color_new, res)
  }
  
  return(color_new[,c("color_scale_score", "color_percentil_range", "color_NSSae")])
}

color_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 101, 18, ifelse (
        db$score >= 99, 17, ifelse (
          db$score >= 94, 16, ifelse (
            db$score >= 87, 15, ifelse (
              db$score >= 82, 14, ifelse (
                db$score >= 79, 13, ifelse (
                  db$score >= 75, 12, ifelse (
                    db$score >= 70, 11, ifelse (
                      db$score >= 62, 10, ifelse (
                        db$score >= 59, 9, ifelse (
                          db$score >= 56, 8, ifelse (
                            db$score >= 49, 7, ifelse (
                              db$score >= 47, 6, ifelse (
                                db$score >= 43, 5, ifelse (
                                  db$score >= 33, 4, ifelse (
                                    db$score >= 22, 3, ifelse (
                                      db$score <= 21, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$color_percentil_range <- with (db, ifelse (
      db$score >= 101, "> 99" , ifelse (
        db$score >= 99, "99" , ifelse (
          db$score >= 94, "98" , ifelse (
            db$score >= 87, "95-97" , ifelse (
              db$score >= 82, "90-94" , ifelse (
                db$score >= 79, "82-89" , ifelse (
                  db$score >= 75, "72-81" , ifelse (
                    db$score >= 70, "60-71" , ifelse (
                      db$score >= 62, "41-59" , ifelse (
                        db$score >= 59, "29-40" , ifelse (
                          db$score >= 56, "19-28" , ifelse (
                            db$score >= 49, "11-18" , ifelse (
                              db$score >= 47, "6-10" , ifelse (
                                db$score >= 43, "3-5" , ifelse (
                                  db$score >= 33, "2" , ifelse (
                                    db$score >= 22, "1" , ifelse (
                                      db$score <= 21, "<1" , NA )))))))))))))))))) 
    
    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 99, 18, ifelse (
        db$score >= 94, 17, ifelse (
          db$score >= 91, 16, ifelse (
            db$score >= 86, 15, ifelse (
              db$score >= 80, 14, ifelse (
                db$score >= 79, 13, ifelse (
                  db$score >= 75, 12, ifelse (
                    db$score >= 68, 11, ifelse (
                      db$score >= 61, 10, ifelse (
                        db$score >= 58, 9, ifelse (
                          db$score >= 52, 8, ifelse (
                            db$score >= 49, 7, ifelse (
                              db$score >= 46, 6, ifelse (
                                db$score >= 37, 5, ifelse (
                                  
                                  db$score >= 33, 3, ifelse (
                                    db$score <= 32, 2, NA )))))))))))))))))

# percentile score

db$color_percentil_range <- with (db, ifelse (
  db$score >= 99, "> 99" , ifelse (
    db$score >= 94, "99" , ifelse (
      db$score >= 91, "98" , ifelse (
        db$score >= 86, "95-97" , ifelse (
          db$score >= 80, "90-94" , ifelse (
            db$score >= 79, "82-89" , ifelse (
              db$score >= 75, "72-81" , ifelse (
                db$score >= 68, "60-71" , ifelse (
                  db$score >= 61, "41-59" , ifelse (
                    db$score >= 58, "29-40" , ifelse (
                      db$score >= 52, "19-28" , ifelse (
                        db$score >= 49, "11-18" , ifelse (
                          db$score >= 46, "6-10" , ifelse (
                            db$score >= 37, "3-5" , ifelse (
                              
                              db$score >= 33, "1" , ifelse (
                                db$score <= 32, "<1" , NA )))))))))))))))))
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 91, 18, ifelse (
        db$score >= 90, 17, ifelse (
          db$score >= 87, 16, ifelse (
            db$score >= 83, 15, ifelse (
              db$score >= 80, 14, ifelse (
                db$score >= 77, 13, ifelse (
                  db$score >= 71, 12, ifelse (
                    db$score >= 66, 11, ifelse (
                      db$score >= 59, 10, ifelse (
                        db$score >= 55, 9, ifelse (
                          db$score >= 50, 8, ifelse (
                            db$score >= 47, 7, ifelse (
                              db$score >= 41, 6, ifelse (
                                db$score >= 37, 5, ifelse (
                                  db$score >= 33, 4, ifelse (
                                    db$score >= 21, 3, ifelse (
                                      db$score <= 20, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$color_percentil_range <- with (db, ifelse (
      db$score >= 91, "> 99" , ifelse (
        db$score >= 90, "99" , ifelse (
          db$score >= 87, "98" , ifelse (
            db$score >= 83, "95-97" , ifelse (
              db$score >= 80, "90-94" , ifelse (
                db$score >= 77, "82-89" , ifelse (
                  db$score >= 71, "72-81" , ifelse (
                    db$score >= 66, "60-71" , ifelse (
                      db$score >= 59, "41-59" , ifelse (
                        db$score >= 55, "29-40" , ifelse (
                          db$score >= 50, "19-28" , ifelse (
                            db$score >= 47, "11-18" , ifelse (
                              db$score >= 41, "6-10" , ifelse (
                                db$score >= 37, "3-5" , ifelse (
                                  db$score >= 33, "2" , ifelse (
                                    db$score >= 21, "1" , ifelse (
                                      db$score <= 20, "<1" , NA ))))))))))))))))))
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 91, 18, ifelse (
        db$score >= 85, 17, ifelse (
          db$score >= 83, 16, ifelse (
            db$score >= 80, 15, ifelse (
              db$score >= 79, 14, ifelse (
                db$score >= 75, 13, ifelse (
                  db$score >= 69, 12, ifelse (
                    db$score >= 63, 11, ifelse (
                      db$score >= 57, 10, ifelse (
                        db$score >= 50, 9, ifelse (
                          db$score >= 48, 8, ifelse (
                            db$score >= 43, 7, ifelse (
                              db$score >= 41, 6, ifelse (
                                db$score >= 37, 5, ifelse (
                                  db$score >= 33, 4, ifelse (
                                    db$score >= 21, 3, ifelse (
                                      db$score <= 20, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$color_percentil_range <- with (db, ifelse (
      db$score >= 91, "> 99" , ifelse (
        db$score >= 85, "99" , ifelse (
          db$score >= 83, "98" , ifelse (
            db$score >= 80, "95-97" , ifelse (
              db$score >= 79, "90-94" , ifelse (
                db$score >= 75, "82-89" , ifelse (
                  db$score >= 69, "72-81" , ifelse (
                    db$score >= 63, "60-71" , ifelse (
                      db$score >= 57, "41-59" , ifelse (
                        db$score >= 50, "29-40" , ifelse (
                          db$score >= 48, "19-28" , ifelse (
                            db$score >= 43, "11-18" , ifelse (
                              db$score >= 41, "6-10" , ifelse (
                                db$score >= 37, "3-5" , ifelse (
                                  db$score >= 33, "2" , ifelse (
                                    db$score >= 21, "1" , ifelse (
                                      db$score <= 20, "<1" , NA ))))))))))))))))))
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 85, 18, ifelse (
        db$score >= 83, 17, ifelse (
          db$score >= 80, 16, ifelse (
            db$score >= 77, 15, ifelse (
              db$score >= 74, 14, ifelse (
                db$score >= 68, 13, ifelse (
                  db$score >= 63, 12, ifelse (
                    db$score >= 60, 11, ifelse (
                      db$score >= 54, 10, ifelse (
                        db$score >= 50, 9, ifelse (
                          db$score >= 46, 8, ifelse (
                            db$score >= 41, 7, ifelse (
                              db$score >= 39, 6, ifelse (
                                db$score >= 27, 5, ifelse (
                                  db$score >= 24, 4, ifelse (
                                    db$score >= 21, 3, ifelse (
                                      db$score <= 20, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$color_percentil_range <- with (db, ifelse (
      db$score >= 85, "> 99" , ifelse (
        db$score >= 83, "99" , ifelse (
          db$score >= 80, "98" , ifelse (
            db$score >= 77, "95-97" , ifelse (
              db$score >= 74, "90-94" , ifelse (
                db$score >= 68, "82-89" , ifelse (
                  db$score >= 63, "72-81" , ifelse (
                    db$score >= 60, "60-71" , ifelse (
                      db$score >= 54, "41-59" , ifelse (
                        db$score >= 50, "29-40" , ifelse (
                          db$score >= 46, "19-28" , ifelse (
                            db$score >= 41, "11-18" , ifelse (
                              db$score >= 39, "6-10" , ifelse (
                                db$score >= 27, "3-5" , ifelse (
                                  db$score >= 24, "2" , ifelse (
                                    db$score >= 21, "1" , ifelse (
                                      db$score <= 20, "<1" , NA ))))))))))))))))))
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 83, 18, ifelse (
        db$score >= 78, 17, ifelse (
          db$score >= 77, 16, ifelse (
            db$score >= 75, 15, ifelse (
              db$score >= 71, 14, ifelse (
                db$score >= 64, 13, ifelse (
                  db$score >= 61, 12, ifelse (
                    db$score >= 57, 11, ifelse (
                      db$score >= 51, 10, ifelse (
                        db$score >= 48, 9, ifelse (
                          db$score >= 44, 8, ifelse (
                            db$score >= 39, 7, ifelse (
                              db$score >= 33, 6, ifelse (
                                db$score >= 24, 5, ifelse (
                                  db$score >= 22, 4, ifelse (
                                    
                                    db$score <= 21, 2, NA )))))))))))))))))

# percentile score

db$color_percentil_range <- with (db, ifelse (
  db$score >= 83, "> 99" , ifelse (
    db$score >= 78, "99" , ifelse (
      db$score >= 77, "98" , ifelse (
        db$score >= 75, "95-97" , ifelse (
          db$score >= 71, "90-94" , ifelse (
            db$score >= 64, "82-89" , ifelse (
              db$score >= 61, "72-81" , ifelse (
                db$score >= 57, "60-71" , ifelse (
                  db$score >= 51, "41-59" , ifelse (
                    db$score >= 48, "29-40" , ifelse (
                      db$score >= 44, "19-28" , ifelse (
                        db$score >= 39, "11-18" , ifelse (
                          db$score >= 33, "6-10" , ifelse (
                            db$score >= 24, "3-5" , ifelse (
                              db$score >= 22, "2" , ifelse (
                                
                                db$score <= 21, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    
    db$color_scale_score <- with ( db, ifelse (
      db$score >=  83, 18, ifelse (
        db$score >= 78, 17, ifelse (
          db$score >= 77, 16, ifelse (
            db$score >= 72, 15, ifelse (
              db$score >= 68, 14, ifelse (
                db$score >= 63 , 13, ifelse (
                  db$score >= 61 , 12, ifelse (
                    db$score >= 56 , 11, ifelse (
                      db$score >= 50 , 10, ifelse (
                        db$score >= 46 , 9, ifelse (
                          db$score >= 41 , 8, ifelse (
                            db$score >= 37 , 7, ifelse (
                              db$score >= 30 , 6, ifelse (
                                db$score >= 24 , 5, ifelse (
                                  db$score >= 22 , 4, ifelse (
                                    
                                    db$score <=  21, 2, NA )))))))))))))))))

# percentile score

db$color_percentil_range <- with (db, ifelse (
  db$score >=  83, "> 99" , ifelse (
    db$score >= 78, "99" , ifelse (
      db$score >= 77, "98" , ifelse (
        db$score >= 72, "95-97" , ifelse (
          db$score >= 68, "90-94" , ifelse (
            db$score >= 63 , "82-89" , ifelse (
              db$score >= 61 , "72-81" , ifelse (
                db$score >= 56 , "60-71" , ifelse (
                  db$score >= 50 , "41-59" , ifelse (
                    db$score >= 46 , "29-40" , ifelse (
                      db$score >= 41 , "19-28" , ifelse (
                        db$score >= 37 , "11-18" , ifelse (
                          db$score >= 30 , "6-10" , ifelse (
                            db$score >= 24 , "3-5" , ifelse (
                              db$score >= 22 , "2" , ifelse (
                                
                                db$score <=  21, "<1" , NA )))))))))))))))))
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 78, 18, ifelse (
        db$score >= 75, 17, ifelse (
          db$score >= 74, 16, ifelse (
            db$score >= 70, 15, ifelse (
              db$score >= 67, 14, ifelse (
                db$score >= 61, 13, ifelse (
                  db$score >= 58, 12, ifelse (
                    db$score >= 55, 11, ifelse (
                      db$score >= 49, 10, ifelse (
                        db$score >= 46, 9, ifelse (
                          db$score >= 40, 8, ifelse (
                            db$score >= 33, 7, ifelse (
                              db$score >= 28, 6, ifelse (
                                db$score >= 22, 5, ifelse (
                                  db$score >= 21, 4, ifelse (
                                    
                                    db$score <= 20, 2, NA )))))))))))))))))

# percentile score

db$color_percentil_range <- with (db, ifelse (
  db$score >= 78, "> 99" , ifelse (
    db$score >= 75, "99" , ifelse (
      db$score >= 74, "98" , ifelse (
        db$score >= 70, "95-97" , ifelse (
          db$score >= 67, "90-94" , ifelse (
            db$score >= 61, "82-89" , ifelse (
              db$score >= 58, "72-81" , ifelse (
                db$score >= 55, "60-71" , ifelse (
                  db$score >= 49, "41-59" , ifelse (
                    db$score >= 46, "29-40" , ifelse (
                      db$score >= 40, "19-28" , ifelse (
                        db$score >= 33, "11-18" , ifelse (
                          db$score >= 28, "6-10" , ifelse (
                            db$score >= 22, "3-5" , ifelse (
                              db$score >= 21, "2" , ifelse (
                                
                                db$score <= 20, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 72, 18, ifelse (
        
        db$score >= 70, 16, ifelse (
          db$score >= 69, 15, ifelse (
            db$score >= 65, 14, ifelse (
              db$score >= 61, 13, ifelse (
                db$score >= 55, 12, ifelse (
                  db$score >= 52, 11, ifelse (
                    db$score >= 47, 10, ifelse (
                      db$score >= 41, 9, ifelse (
                        db$score >= 36, 8, ifelse (
                          db$score >= 33, 7, ifelse (
                            db$score >= 28, 6, ifelse (
                              db$score >= 22, 5, ifelse (
                                
                                db$score >= 21, 3, ifelse (
                                  db$score <= 20, 2, NA ))))))))))))))))

# percentile score

db$color_percentil_range <- with (db, ifelse (
  db$score >= 72, "> 99" , ifelse (
    
    db$score >= 70, "98" , ifelse (
      db$score >= 69, "95-97" , ifelse (
        db$score >= 65, "90-94" , ifelse (
          db$score >= 61, "82-89" , ifelse (
            db$score >= 55, "72-81" , ifelse (
              db$score >= 52, "60-71" , ifelse (
                db$score >= 47, "41-59" , ifelse (
                  db$score >= 41, "29-40" , ifelse (
                    db$score >= 36, "19-28" , ifelse (
                      db$score >= 33, "11-18" , ifelse (
                        db$score >= 28, "6-10" , ifelse (
                          db$score >= 22, "3-5" , ifelse (
                            
                            db$score >= 21, "1" , ifelse (
                              db$score <= 20, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$color_scale_score <- with ( db, ifelse (
      db$score >= 72, 18, ifelse (
        
        db$score >= 69, 16, ifelse (
          db$score >= 67, 15, ifelse (
            db$score >= 61, 14, ifelse (
              db$score >= 59, 13, ifelse (
                db$score >= 55, 12, ifelse (
                  db$score >= 50, 11, ifelse (
                    db$score >= 47, 10, ifelse (
                      db$score >= 41, 9, ifelse (
                        db$score >= 36, 8, ifelse (
                          db$score >= 33, 7, ifelse (
                            db$score >= 22, 6, ifelse (
                              db$score >= 19, 5, ifelse (
                                db$score >= 18, 4, ifelse (
                                  
                                  db$score <= 17, 2, NA ))))))))))))))))

# percentile score

db$color_percentil_range <- with (db, ifelse (
  db$score >= 72, "> 99" , ifelse (
    
    db$score >= 69, "98" , ifelse (
      db$score >= 67, "95-97" , ifelse (
        db$score >= 61, "90-94" , ifelse (
          db$score >= 59, "82-89" , ifelse (
            db$score >= 55, "72-81" , ifelse (
              db$score >= 50, "60-71" , ifelse (
                db$score >= 47, "41-59" , ifelse (
                  db$score >= 41, "29-40" , ifelse (
                    db$score >= 36, "19-28" , ifelse (
                      db$score >= 33, "11-18" , ifelse (
                        db$score >= 22, "6-10" , ifelse (
                          db$score >= 19, "3-5" , ifelse (
                            db$score >= 18, "2" , ifelse (
                              
                              db$score <= 17, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$color_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 2, db$color_scale_score + 2, ifelse(
      db$education_years >= 3  & db$education_years <= 7, db$color_scale_score + 1, ifelse(
        db$education_years >= 8  & db$education_years <= 12, db$color_scale_score, ifelse(
          db$education_years >= 13  & db$education_years <= 16, db$color_scale_score - 1, ifelse(
            db$education_years >= 17  & db$education_years <= 20, db$color_scale_score - 2, ifelse(
            )))))))
  
  
  # NSSae
  db$color_NSSae <- db$color_scale_score - (0.2099*(db$color_education_years_adj-12)) 
  
  return(db)
}
