# trail making test A 
#
# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 

tmtb <- function(score, age, education_years){
  
  tmtb_db <- data.frame(score = score, age = age, education_years = education_years)
  tmtb_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(tmtb_db)) {
    res <- tmtb_scale_score(score = tmtb_db[i, "score"], 
                              age = tmtb_db[i, "age"],
                              education_years = tmtb_db[i, "education_years"])
    tmtb_new <- rbind(tmtb_new, res)
  }
  
  return(tmtb_new[,c("tmtb_scale_score", "tmtb_percentil_range", "tmtb_NSSae")])
}

tmtb_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 41, 18, ifelse (
        db$score <= 42, 17, ifelse (
          db$score <= 44, 16, ifelse (
            db$score <= 50, 15, ifelse (
              db$score <= 54, 14, ifelse (
                db$score <= 63, 13, ifelse (
                  db$score <= 70, 12, ifelse (
                    db$score <= 79, 11, ifelse (
                      db$score <= 107, 10, ifelse (
                        db$score <= 122, 9, ifelse (
                          db$score <= 137, 8, ifelse (
                            db$score <= 177, 7, ifelse (
                              db$score <= 222, 6, ifelse (
                                db$score <= 380, 5, ifelse (
                                  db$score <= 405, 4, ifelse (
                                    db$score <= 479, 3, ifelse (
                                      db$score >= 480, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmtb_percentil_range <- with (db, ifelse (
      db$score <= 41, "> 99" , ifelse (
        db$score <= 42, "99" , ifelse (
          db$score <= 44, "98" , ifelse (
            db$score <= 50, "95-97" , ifelse (
              db$score <= 54, "90-94" , ifelse (
                db$score <= 63, "82-89" , ifelse (
                  db$score <= 70, "72-81" , ifelse (
                    db$score <= 79, "60-71" , ifelse (
                      db$score <= 107, "41-59" , ifelse (
                        db$score <= 122, "29-40" , ifelse (
                          db$score <= 137, "19-28" , ifelse (
                            db$score <= 177, "11-18" , ifelse (
                              db$score <= 222, "6-10" , ifelse (
                                db$score <= 380, "3-5" , ifelse (
                                  db$score <= 405, "2" , ifelse (
                                    db$score <= 479, "1" , ifelse (
                                      db$score >= 480, "<1" , NA ))))))))))))))))))
    
    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 41, 18, ifelse (
        db$score <= 44, 17, ifelse (
          
          db$score <= 52, 15, ifelse (
            db$score <= 55, 14, ifelse (
              db$score <= 65, 13, ifelse (
                db$score <= 72, 12, ifelse (
                  db$score <= 88, 11, ifelse (
                    db$score <= 113, 10, ifelse (
                      db$score <= 128, 9, ifelse (
                        db$score <= 150, 8, ifelse (
                          db$score <= 196, 7, ifelse (
                            db$score <= 230, 6, ifelse (
                              db$score <= 380, 5, ifelse (
                                db$score <= 382, 4, ifelse (
                                  db$score <= 405, 3, ifelse (
                                    db$score >= 406, 2, NA )))))))))))))))))

# percentile score

db$tmtb_percentil_range <- with (db, ifelse (
  db$score <= 41, "> 99" , ifelse (
    db$score <= 44, "99" , ifelse (
      
      db$score <= 52, "95-97" , ifelse (
        db$score <= 55, "90-94" , ifelse (
          db$score <= 65, "82-89" , ifelse (
            db$score <= 72, "72-81" , ifelse (
              db$score <= 88, "60-71" , ifelse (
                db$score <= 113, "41-59" , ifelse (
                  db$score <= 128, "29-40" , ifelse (
                    db$score <= 150, "19-28" , ifelse (
                      db$score <= 196, "11-18" , ifelse (
                        db$score <= 230, "6-10" , ifelse (
                          db$score <= 380, "3-5" , ifelse (
                            db$score <= 382, "2" , ifelse (
                              db$score <= 405, "1" , ifelse (
                                db$score >= 406, "<1" , NA )))))))))))))))))
    
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 46, 18, ifelse (
        db$score <= 47, 17, ifelse (
          db$score <= 48, 16, ifelse (
            db$score <= 52, 15, ifelse (
              db$score <= 61, 14, ifelse (
                db$score <= 70, 13, ifelse (
                  db$score <= 78, 12, ifelse (
                    db$score <= 100, 11, ifelse (
                      db$score <= 118, 10, ifelse (
                        db$score <= 134, 9, ifelse (
                          db$score <= 160, 8, ifelse (
                            db$score <= 205, 7, ifelse (
                              db$score <= 252, 6, ifelse (
                                db$score <= 380, 5, ifelse (
                                  db$score <= 382, 4, ifelse (
                                    db$score <= 405, 3, ifelse (
                                      db$score >= 406, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmtb_percentil_range <- with (db, ifelse (
      db$score <= 46, "> 99" , ifelse (
        db$score <= 47, "99" , ifelse (
          db$score <= 48, "98" , ifelse (
            db$score <= 52, "95-97" , ifelse (
              db$score <= 61, "90-94" , ifelse (
                db$score <= 70, "82-89" , ifelse (
                  db$score <= 78, "72-81" , ifelse (
                    db$score <= 100, "60-71" , ifelse (
                      db$score <= 118, "41-59" , ifelse (
                        db$score <= 134, "29-40" , ifelse (
                          db$score <= 160, "19-28" , ifelse (
                            db$score <= 205, "11-18" , ifelse (
                              db$score <= 252, "6-10" , ifelse (
                                db$score <= 380, "3-5" , ifelse (
                                  db$score <= 382, "2" , ifelse (
                                    db$score <= 405, "1" , ifelse (
                                      db$score >= 406, "<1" , NA ))))))))))))))))))
    
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 41, 18, ifelse (
        db$score <= 42, 17, ifelse (
          db$score <= 47, 16, ifelse (
            db$score <= 55, 15, ifelse (
              db$score <= 63, 14, ifelse (
                db$score <= 69, 13, ifelse (
                  db$score <= 81, 12, ifelse (
                    db$score <= 100, 11, ifelse (
                      db$score <= 130, 10, ifelse (
                        db$score <= 144, 9, ifelse (
                          db$score <= 190, 8, ifelse (
                            db$score <= 221, 7, ifelse (
                              db$score <= 300, 6, ifelse (
                                db$score <= 401, 5, ifelse (
                                  db$score <= 448, 4, ifelse (
                                    db$score <= 480, 3, ifelse (
                                      db$score >= 481, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmtb_percentil_range <- with (db, ifelse (
      db$score <= 41, "> 99" , ifelse (
        db$score <= 42, "99" , ifelse (
          db$score <= 47, "98" , ifelse (
            db$score <= 55, "95-97" , ifelse (
              db$score <= 63, "90-94" , ifelse (
                db$score <= 69, "82-89" , ifelse (
                  db$score <= 81, "72-81" , ifelse (
                    db$score <= 100, "60-71" , ifelse (
                      db$score <= 130, "41-59" , ifelse (
                        db$score <= 144, "29-40" , ifelse (
                          db$score <= 190, "19-28" , ifelse (
                            db$score <= 221, "11-18" , ifelse (
                              db$score <= 300, "6-10" , ifelse (
                                db$score <= 401, "3-5" , ifelse (
                                  db$score <= 448, "2" , ifelse (
                                    db$score <= 480, "1" , ifelse (
                                      db$score >= 481, "<1" , NA ))))))))))))))))))
    
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 42, 18, ifelse (
        db$score <= 47, 17, ifelse (
          db$score <= 59, 16, ifelse (
            db$score <= 67, 15, ifelse (
              db$score <= 71, 14, ifelse (
                db$score <= 78, 13, ifelse (
                  db$score <= 91, 12, ifelse (
                    db$score <= 105, 11, ifelse (
                      db$score <= 136, 10, ifelse (
                        db$score <= 158, 9, ifelse (
                          db$score <= 192, 8, ifelse (
                            db$score <= 221, 7, ifelse (
                              db$score <= 266, 6, ifelse (
                                db$score <= 317, 5, ifelse (
                                  db$score <= 382, 4, ifelse (
                                    db$score <= 401, 3, ifelse (
                                      db$score >= 402, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmtb_percentil_range <- with (db, ifelse (
      db$score <= 42, "> 99" , ifelse (
        db$score <= 47, "99" , ifelse (
          db$score <= 59, "98" , ifelse (
            db$score <= 67, "95-97" , ifelse (
              db$score <= 71, "90-94" , ifelse (
                db$score <= 78, "82-89" , ifelse (
                  db$score <= 91, "72-81" , ifelse (
                    db$score <= 105, "60-71" , ifelse (
                      db$score <= 136, "41-59" , ifelse (
                        db$score <= 158, "29-40" , ifelse (
                          db$score <= 192, "19-28" , ifelse (
                            db$score <= 221, "11-18" , ifelse (
                              db$score <= 266, "6-10" , ifelse (
                                db$score <= 317, "3-5" , ifelse (
                                  db$score <= 382, "2" , ifelse (
                                    db$score <= 401, "1" , ifelse (
                                      db$score >= 402, "<1" , NA ))))))))))))))))))
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 42, 18, ifelse (
        db$score <= 60, 17, ifelse (
          
          db$score <= 66, 15, ifelse (
            db$score <= 72, 14, ifelse (
              db$score <= 81, 13, ifelse (
                db$score <= 96, 12, ifelse (
                  db$score <= 113, 11, ifelse (
                    db$score <= 138, 10, ifelse (
                      db$score <= 167, 9, ifelse (
                        db$score <= 193, 8, ifelse (
                          db$score <= 240, 7, ifelse (
                            db$score <= 308, 6, ifelse (
                              db$score <= 420, 5, ifelse (
                                db$score <= 448, 4, ifelse (
                                  db$score <= 500, 3, ifelse (
                                    db$score >= 501, 2, NA )))))))))))))))))

# percentile score

db$tmtb_percentil_range <- with (db, ifelse (
  db$score <= 42, "> 99" , ifelse (
    db$score <= 60, "99" , ifelse (
      
      db$score <= 66, "95-97" , ifelse (
        db$score <= 72, "90-94" , ifelse (
          db$score <= 81, "82-89" , ifelse (
            db$score <= 96, "72-81" , ifelse (
              db$score <= 113, "60-71" , ifelse (
                db$score <= 138, "41-59" , ifelse (
                  db$score <= 167, "29-40" , ifelse (
                    db$score <= 193, "19-28" , ifelse (
                      db$score <= 240, "11-18" , ifelse (
                        db$score <= 308, "6-10" , ifelse (
                          db$score <= 420, "3-5" , ifelse (
                            db$score <= 448, "2" , ifelse (
                              db$score <= 500, "1" , ifelse (
                                db$score >= 501, "<1" , NA ))))))))))))))))) 
    
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 60, 18, ifelse (
        
        db$score <= 63, 16, ifelse (
          db$score <= 68, 15, ifelse (
            db$score <= 75, 14, ifelse (
              db$score <= 91, 13, ifelse (
                db$score <= 104, 12, ifelse (
                  db$score <= 115, 11, ifelse (
                    db$score <= 148, 10, ifelse (
                      db$score <= 190, 9, ifelse (
                        db$score <= 213, 8, ifelse (
                          db$score <= 273, 7, ifelse (
                            db$score <= 336, 6, ifelse (
                              db$score <= 448, 5, ifelse (
                                db$score <= 461, 4, ifelse (
                                  db$score <= 500, 3, ifelse (
                                    db$score >= 501, 2, NA )))))))))))))))))

# percentile score

db$tmtb_percentil_range <- with (db, ifelse (
  db$score <= 60, "> 99" , ifelse (
    
    db$score <= 63, "98" , ifelse (
      db$score <= 68, "95-97" , ifelse (
        db$score <= 75, "90-94" , ifelse (
          db$score <= 91, "82-89" , ifelse (
            db$score <= 104, "72-81" , ifelse (
              db$score <= 115, "60-71" , ifelse (
                db$score <= 148, "41-59" , ifelse (
                  db$score <= 190, "29-40" , ifelse (
                    db$score <= 213, "19-28" , ifelse (
                      db$score <= 273, "11-18" , ifelse (
                        db$score <= 336, "6-10" , ifelse (
                          db$score <= 448, "3-5" , ifelse (
                            db$score <= 461, "2" , ifelse (
                              db$score <= 500, "1" , ifelse (
                                db$score >= 501, "<1" , NA ))))))))))))))))) 
    
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 59, 18, ifelse (
        db$score <= 60, 17, ifelse (
          db$score <= 63, 16, ifelse (
            db$score <= 74, 15, ifelse (
              db$score <= 82, 14, ifelse (
                db$score <= 95, 13, ifelse (
                  db$score <= 106, 12, ifelse (
                    db$score <= 120, 11, ifelse (
                      db$score <= 159, 10, ifelse (
                        db$score <= 193, 9, ifelse (
                          db$score <= 220, 8, ifelse (
                            db$score <= 280, 7, ifelse (
                              db$score <= 364, 6, ifelse (
                                db$score <= 461, 5, ifelse (
                                  db$score <= 500, 4, ifelse (
                                    db$score <= 539, 3, ifelse (
                                      db$score >= 540, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmtb_percentil_range <- with (db, ifelse (
      db$score <= 59, "> 99" , ifelse (
        db$score <= 60, "99" , ifelse (
          db$score <= 63, "98" , ifelse (
            db$score <= 74, "95-97" , ifelse (
              db$score <= 82, "90-94" , ifelse (
                db$score <= 95, "82-89" , ifelse (
                  db$score <= 106, "72-81" , ifelse (
                    db$score <= 120, "60-71" , ifelse (
                      db$score <= 159, "41-59" , ifelse (
                        db$score <= 193, "29-40" , ifelse (
                          db$score <= 220, "19-28" , ifelse (
                            db$score <= 280, "11-18" , ifelse (
                              db$score <= 364, "6-10" , ifelse (
                                db$score <= 461, "3-5" , ifelse (
                                  db$score <= 500, "2" , ifelse (
                                    db$score <= 539, "1" , ifelse (
                                      db$score >= 540, "<1" , NA )))))))))))))))))) 
    
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 62, 18, ifelse (
        db$score <= 63, 17, ifelse (
          db$score <= 79, 16, ifelse (
            db$score <= 82, 15, ifelse (
              db$score <= 83, 14, ifelse (
                db$score <= 106, 13, ifelse (
                  db$score <= 119, 12, ifelse (
                    db$score <= 141, 11, ifelse (
                      db$score <= 190, 10, ifelse (
                        db$score <= 214, 9, ifelse (
                          db$score <= 240, 8, ifelse (
                            db$score <= 316, 7, ifelse (
                              db$score <= 352, 6, ifelse (
                                db$score <= 428, 5, ifelse (
                                  db$score <= 461, 4, ifelse (
                                    db$score <= 538, 3, ifelse (
                                      db$score >= 539, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$tmtb_percentil_range <- with (db, ifelse (
      db$score <= 62, "> 99" , ifelse (
        db$score <= 63, "99" , ifelse (
          db$score <= 79, "98" , ifelse (
            db$score <= 82, "95-97" , ifelse (
              db$score <= 83, "90-94" , ifelse (
                db$score <= 106, "82-89" , ifelse (
                  db$score <= 119, "72-81" , ifelse (
                    db$score <= 141, "60-71" , ifelse (
                      db$score <= 190, "41-59" , ifelse (
                        db$score <= 214, "29-40" , ifelse (
                          db$score <= 240, "19-28" , ifelse (
                            db$score <= 316, "11-18" , ifelse (
                              db$score <= 352, "6-10" , ifelse (
                                db$score <= 428, "3-5" , ifelse (
                                  db$score <= 461, "2" , ifelse (
                                    db$score <= 538, "1" , ifelse (
                                      db$score >= 539, "<1" , NA )))))))))))))))))) 
    
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$tmtb_scale_score <- with ( db, ifelse (
      db$score <= 79, 18, ifelse (
        
        db$score <= 80, 16, ifelse (
          db$score <= 83, 15, ifelse (
            db$score <= 98, 14, ifelse (
              db$score <= 114, 13, ifelse (
                db$score <= 120, 12, ifelse (
                  db$score <= 163, 11, ifelse (
                    db$score <= 210, 10, ifelse (
                      db$score <= 235, 9, ifelse (
                        db$score <= 298, 8, ifelse (
                          db$score <= 352, 7, ifelse (
                            db$score <= 367, 6, ifelse (
                              db$score <= 428, 5, ifelse (
                                db$score <= 460, 4, ifelse (
                                  
                                  db$score >= 461, 2, NA ))))))))))))))))

# percentile score

db$tmtb_percentil_range <- with (db, ifelse (
  db$score <= 79, "> 99" , ifelse (
    
    db$score <= 80, "98" , ifelse (
      db$score <= 83, "95-97" , ifelse (
        db$score <= 98, "90-94" , ifelse (
          db$score <= 114, "82-89" , ifelse (
            db$score <= 120, "72-81" , ifelse (
              db$score <= 163, "60-71" , ifelse (
                db$score <= 210, "41-59" , ifelse (
                  db$score <= 235, "29-40" , ifelse (
                    db$score <= 298, "19-28" , ifelse (
                      db$score <= 352, "11-18" , ifelse (
                        db$score <= 367, "6-10" , ifelse (
                          db$score <= 428, "3-5" , ifelse (
                            db$score <= 460, "2" , ifelse (
                              
                              db$score >= 461, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$tmtb_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$tmtb_scale_score + 2, ifelse(
    db$education_years >= 1  & db$education_years <= 4, db$tmtb_scale_score + 1, ifelse(
      db$education_years >= 5  & db$education_years <= 8, db$tmtb_scale_score, ifelse(
        db$education_years >= 9  & db$education_years <= 11, db$tmtb_scale_score - 1, ifelse(
          db$education_years >= 12  & db$education_years <= 15, db$tmtb_scale_score - 2, ifelse(
            db$education_years >= 16  & db$education_years <= 18, db$tmtb_scale_score- 3, ifelse(
              db$education_years >= 19  & db$education_years <= 20, db$tmtb_scale_score- 4, ifelse(
            )))))))))
  
  
  # NSSae
  db$tmtb_NSSae <- db$tmtb_scale_score - (-0.27320*(db$tmtb_education_years_adj-12)) 
  
  return(db)
}
