# Tower of London - Total execution time

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

# 
TOL_ET <- function(score, age, education_years){
  
  TOL_ET_db <- data.frame(score = score, age = age, education_years = education_years)
  TOL_ET_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(TOL_ET_db)) {
    res <- TOL_ET_scale_score(score = TOL_ET_db[i, "score"], 
                              age = TOL_ET_db[i, "age"],
                              education_years = TOL_ET_db[i, "education_years"])
    TOL_ET_new <- rbind(TOL_ET_new, res)
  }
  
  return(TOL_ET_new[,c("TOL_ET_scale_score", "TOL_ET_percentil_range", "TOL_ET_NSSae")])
}

TOL_ET_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 92, 18, ifelse (
        db$score <= 99, 17, ifelse (
          db$score <= 114, 16, ifelse (
            db$score <= 124, 15, ifelse (
              db$score <= 138, 14, ifelse (
                db$score <= 156, 13, ifelse (
                  db$score <= 175, 12, ifelse (
                    db$score <= 221, 11, ifelse (
                      db$score <= 266, 10, ifelse (
                        db$score <= 316, 9, ifelse (
                          db$score <= 348, 8, ifelse (
                            db$score <= 371, 7, ifelse (
                              db$score <= 451, 6, ifelse (
                                db$score <= 508, 5, ifelse (
                                  db$score <= 621, 4, ifelse (
                                    db$score <= 679, 3, ifelse (
                                      db$score >= 680, 2, NA )))))))))))))))))))
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 92, "> 99" , ifelse (
        db$score <= 99, "99" , ifelse (
          db$score <= 114, "98" , ifelse (
            db$score <= 124, "95-97" , ifelse (
              db$score <= 138, "90-94" , ifelse (
                db$score <= 156, "82-89" , ifelse (
                  db$score <= 175, "72-81" , ifelse (
                    db$score <= 221, "60-71" , ifelse (
                      db$score <= 266, "41-59" , ifelse (
                        db$score <= 316, "29-40" , ifelse (
                          db$score <= 348, "19-28" , ifelse (
                            db$score <= 371, "11-18" , ifelse (
                              db$score <= 451, "6-10" , ifelse (
                                db$score <= 508, "3-5" , ifelse (
                                  db$score <= 621, "2" , ifelse (
                                    db$score <= 679, "1" , ifelse (
                                      db$score >= 680, "<1" , NA )))))))))))))))))) )
    
    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 114, 18, ifelse (
        db$score <= 124, 17, ifelse (
          db$score <= 127, 16, ifelse (
            db$score <= 136, 15, ifelse (
              db$score <= 155, 14, ifelse (
                db$score <= 172, 13, ifelse (
                  db$score <= 198, 12, ifelse (
                    db$score <= 233, 11, ifelse (
                      db$score <= 266, 10, ifelse (
                        db$score <= 319, 9, ifelse (
                          db$score <= 356, 8, ifelse (
                            db$score <= 415, 7, ifelse (
                              db$score <= 482, 6, ifelse (
                                db$score <= 523, 5, ifelse (
                                  db$score <= 621, 4, ifelse (
                                    db$score <= 679, 3, ifelse (
                                      db$score >= 680, 2, NA )))))))))))))))))))
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 114, "> 99" , ifelse (
        db$score <= 124, "99" , ifelse (
          db$score <= 127, "98" , ifelse (
            db$score <= 136, "95-97" , ifelse (
              db$score <= 155, "90-94" , ifelse (
                db$score <= 172, "82-89" , ifelse (
                  db$score <= 198, "72-81" , ifelse (
                    db$score <= 233, "60-71" , ifelse (
                      db$score <= 266, "41-59" , ifelse (
                        db$score <= 319, "29-40" , ifelse (
                          db$score <= 356, "19-28" , ifelse (
                            db$score <= 415, "11-18" , ifelse (
                              db$score <= 482, "6-10" , ifelse (
                                db$score <= 523, "3-5" , ifelse (
                                  db$score <= 621, "2" , ifelse (
                                    db$score <= 679, "1" , ifelse (
                                      db$score >= 680, "<1" , NA )))))))))))))))))) )
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 124, 18, ifelse (
        db$score <= 126, 17, ifelse (
          db$score <= 127, 16, ifelse (
            db$score <= 143, 15, ifelse (
              db$score <= 153, 14, ifelse (
                db$score <= 173, 13, ifelse (
                  db$score <= 198, 12, ifelse (
                    db$score <= 233, 11, ifelse (
                      db$score <= 276, 10, ifelse (
                        db$score <= 330, 9, ifelse (
                          db$score <= 368, 8, ifelse (
                            db$score <= 430, 7, ifelse (
                              db$score <= 482, 6, ifelse (
                                db$score <= 621, 5, ifelse (
                                  db$score <= 679, 4, ifelse (
                                    db$score <= 739, 3, ifelse (
                                      db$score >= 740, 2, NA )))))))))))))))))))
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 124, "> 99" , ifelse (
        db$score <= 126, "99" , ifelse (
          db$score <= 127, "98" , ifelse (
            db$score <= 143, "95-97" , ifelse (
              db$score <= 153, "90-94" , ifelse (
                db$score <= 173, "82-89" , ifelse (
                  db$score <= 198, "72-81" , ifelse (
                    db$score <= 233, "60-71" , ifelse (
                      db$score <= 276, "41-59" , ifelse (
                        db$score <= 330, "29-40" , ifelse (
                          db$score <= 368, "19-28" , ifelse (
                            db$score <= 430, "11-18" , ifelse (
                              db$score <= 482, "6-10" , ifelse (
                                db$score <= 621, "3-5" , ifelse (
                                  db$score <= 679, "2" , ifelse (
                                    db$score <= 739, "1" , ifelse (
                                      db$score >= 740, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 126, 18, ifelse (
        db$score <= 127, 17, ifelse (
          db$score <= 129, 16, ifelse (
            db$score <= 149, 15, ifelse (
              db$score <= 168, 14, ifelse (
                db$score <= 181, 13, ifelse (
                  db$score <= 214, 12, ifelse (
                    db$score <= 243, 11, ifelse (
                      db$score <= 280, 10, ifelse (
                        db$score <= 340, 9, ifelse (
                          db$score <= 381, 8, ifelse (
                            db$score <= 459, 7, ifelse (
                              db$score <= 514, 6, ifelse (
                                db$score <= 679, 5, ifelse (
                                  db$score <= 739, 4, ifelse (
                                    db$score <= 741, 3, ifelse (
                                      db$score >= 742, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 126, "> 99" , ifelse (
        db$score <= 127, "99" , ifelse (
          db$score <= 129, "98" , ifelse (
            db$score <= 149, "95-97" , ifelse (
              db$score <= 168, "90-94" , ifelse (
                db$score <= 181, "82-89" , ifelse (
                  db$score <= 214, "72-81" , ifelse (
                    db$score <= 243, "60-71" , ifelse (
                      db$score <= 280, "41-59" , ifelse (
                        db$score <= 340, "29-40" , ifelse (
                          db$score <= 381, "19-28" , ifelse (
                            db$score <= 459, "11-18" , ifelse (
                              db$score <= 514, "6-10" , ifelse (
                                db$score <= 679, "3-5" , ifelse (
                                  db$score <= 739, "2" , ifelse (
                                    db$score <= 741, "1" , ifelse (
                                      db$score >= 742, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 126, 18, ifelse (
        db$score <= 129, 17, ifelse (
          db$score <= 134, 16, ifelse (
            db$score <= 152, 15, ifelse (
              db$score <= 168, 14, ifelse (
                db$score <= 179, 13, ifelse (
                  db$score <= 213, 12, ifelse (
                    db$score <= 242, 11, ifelse (
                      db$score <= 309, 10, ifelse (
                        db$score <= 353, 9, ifelse (
                          db$score <= 395, 8, ifelse (
                            db$score <= 462, 7, ifelse (
                              db$score <= 523, 6, ifelse (
                                db$score <= 670, 5, ifelse (
                                  db$score <= 739, 4, ifelse (
                                    db$score <= 741, 3, ifelse (
                                      db$score >= 742, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 126, "> 99" , ifelse (
        db$score <= 129, "99" , ifelse (
          db$score <= 134, "98" , ifelse (
            db$score <= 152, "95-97" , ifelse (
              db$score <= 168, "90-94" , ifelse (
                db$score <= 179, "82-89" , ifelse (
                  db$score <= 213, "72-81" , ifelse (
                    db$score <= 242, "60-71" , ifelse (
                      db$score <= 309, "41-59" , ifelse (
                        db$score <= 353, "29-40" , ifelse (
                          db$score <= 395, "19-28" , ifelse (
                            db$score <= 462, "11-18" , ifelse (
                              db$score <= 523, "6-10" , ifelse (
                                db$score <= 670, "3-5" , ifelse (
                                  db$score <= 739, "2" , ifelse (
                                    db$score <= 741, "1" , ifelse (
                                      db$score >= 742, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 126, 18, ifelse (
        db$score <= 129, 17, ifelse (
          db$score <= 134, 16, ifelse (
            db$score <= 152, 15, ifelse (
              db$score <= 170, 14, ifelse (
                db$score <= 180, 13, ifelse (
                  db$score <= 215, 12, ifelse (
                    db$score <= 267, 11, ifelse (
                      db$score <= 334, 10, ifelse (
                        db$score <= 371, 9, ifelse (
                          db$score <= 451, 8, ifelse (
                            db$score <= 518, 7, ifelse (
                              db$score <= 622, 6, ifelse (
                                db$score <= 739, 5, ifelse (
                                  db$score <= 741, 4, ifelse (
                                    db$score <= 792, 3, ifelse (
                                      db$score >= 793, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 126, "> 99" , ifelse (
        db$score <= 129, "99" , ifelse (
          db$score <= 134, "98" , ifelse (
            db$score <= 152, "95-97" , ifelse (
              db$score <= 170, "90-94" , ifelse (
                db$score <= 180, "82-89" , ifelse (
                  db$score <= 215, "72-81" , ifelse (
                    db$score <= 267, "60-71" , ifelse (
                      db$score <= 334, "41-59" , ifelse (
                        db$score <= 371, "29-40" , ifelse (
                          db$score <= 451, "19-28" , ifelse (
                            db$score <= 518, "11-18" , ifelse (
                              db$score <= 622, "6-10" , ifelse (
                                db$score <= 739, "3-5" , ifelse (
                                  db$score <= 741, "2" , ifelse (
                                    db$score <= 792, "1" , ifelse (
                                      db$score >= 793, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <=  151, 18, ifelse (
        db$score <= 157, 17, ifelse (
          db$score <= 159, 16, ifelse (
            db$score <= 170, 15, ifelse (
              db$score <= 179, 14, ifelse (
                db$score <= 202, 13, ifelse (
                  db$score <= 238, 12, ifelse (
                    db$score <= 273, 11, ifelse (
                      db$score <= 349, 10, ifelse (
                        db$score <= 409, 9, ifelse (
                          db$score <= 482, 8, ifelse (
                            db$score <= 549, 7, ifelse (
                              db$score <= 622, 6, ifelse (
                                db$score <= 724, 5, ifelse (
                                  db$score <= 741, 4, ifelse (
                                    db$score <= 792, 3, ifelse (
                                      db$score >=  793, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <=  151, "> 99" , ifelse (
        db$score <= 157, "99" , ifelse (
          db$score <= 159, "98" , ifelse (
            db$score <= 170, "95-97" , ifelse (
              db$score <= 179, "90-94" , ifelse (
                db$score <= 202, "82-89" , ifelse (
                  db$score <= 238, "72-81" , ifelse (
                    db$score <= 273, "60-71" , ifelse (
                      db$score <= 349, "41-59" , ifelse (
                        db$score <= 409, "29-40" , ifelse (
                          db$score <= 482, "19-28" , ifelse (
                            db$score <= 549, "11-18" , ifelse (
                              db$score <= 622, "6-10" , ifelse (
                                db$score <= 724, "3-5" , ifelse (
                                  db$score <= 741, "2" , ifelse (
                                    db$score <= 792, "1" , ifelse (
                                      db$score >=  793, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 183, 18, ifelse (
        db$score <= 159, 17, ifelse (
          db$score <= 165, 16, ifelse (
            db$score <= 171, 15, ifelse (
              db$score <= 185, 14, ifelse (
                db$score <= 213, 13, ifelse (
                  db$score <= 250, 12, ifelse (
                    db$score <= 287, 11, ifelse (
                      db$score <= 353, 10, ifelse (
                        db$score <= 442, 9, ifelse (
                          db$score <= 505, 8, ifelse (
                            db$score <= 570, 7, ifelse (
                              db$score <= 622, 6, ifelse (
                                db$score <= 696, 5, ifelse (
                                  db$score <= 724, 4, ifelse (
                                    db$score <= 792, 3, ifelse (
                                      db$score >= 793, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_ET_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 183, "> 99" , ifelse (
        db$score <= 159, "99" , ifelse (
          db$score <= 165, "98" , ifelse (
            db$score <= 171, "95-97" , ifelse (
              db$score <= 185, "90-94" , ifelse (
                db$score <= 213, "82-89" , ifelse (
                  db$score <= 250, "72-81" , ifelse (
                    db$score <= 287, "60-71" , ifelse (
                      db$score <= 353, "41-59" , ifelse (
                        db$score <= 442, "29-40" , ifelse (
                          db$score <= 505, "19-28" , ifelse (
                            db$score <= 570, "11-18" , ifelse (
                              db$score <= 622, "6-10" , ifelse (
                                db$score <= 696, "3-5" , ifelse (
                                  db$score <= 724, "2" , ifelse (
                                    db$score <= 792, "1" , ifelse (
                                      db$score >= 793, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 143, 18, ifelse (
        
        db$score <= 171, 16, ifelse (
          db$score <= 188, 15, ifelse (
            db$score <= 213, 14, ifelse (
              db$score <= 250, 13, ifelse (
                db$score <= 271, 12, ifelse (
                  db$score <= 300, 11, ifelse (
                    db$score <= 406, 10, ifelse (
                      db$score <= 472, 9, ifelse (
                        db$score <= 509, 8, ifelse (
                          db$score <= 594, 7, ifelse (
                            db$score <= 696, 6, ifelse (
                              db$score <= 792, 5, ifelse (
                                
                                db$score <= 793, 3, ifelse (
                                  db$score >= 794, 2, NA )))))))))))))))) )

# percentile score

db$TOL_ET_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 143, "> 99" , ifelse (
    
    db$score <= 171, "98" , ifelse (
      db$score <= 188, "95-97" , ifelse (
        db$score <= 213, "90-94" , ifelse (
          db$score <= 250, "82-89" , ifelse (
            db$score <= 271, "72-81" , ifelse (
              db$score <= 300, "60-71" , ifelse (
                db$score <= 406, "41-59" , ifelse (
                  db$score <= 472, "29-40" , ifelse (
                    db$score <= 509, "19-28" , ifelse (
                      db$score <= 594, "11-18" , ifelse (
                        db$score <= 696, "6-10" , ifelse (
                          db$score <= 792, "3-5" , ifelse (
                            
                            db$score <= 793, "1" , ifelse (
                              db$score >= 794, "<1" , NA )))))))))))))))) )
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$TOL_ET_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 188, 18, ifelse (
        
        db$score <= 202, 16, ifelse (
          db$score <= 213, 15, ifelse (
            db$score <= 236, 14, ifelse (
              db$score <= 250, 13, ifelse (
                db$score <= 271, 12, ifelse (
                  db$score <= 302, 11, ifelse (
                    db$score <= 406, 10, ifelse (
                      db$score <= 498, 9, ifelse (
                        db$score <= 570, 8, ifelse (
                          db$score <= 599, 7, ifelse (
                            db$score <= 708, 6, ifelse (
                              db$score <= 872, 5, ifelse (
                                db$score <= 873, 4, ifelse (
                                  
                                  db$score >= 874, 2, NA )))))))))))))))) )

# percentile score

db$TOL_ET_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 188, "> 99" , ifelse (
    
    db$score <= 202, "98" , ifelse (
      db$score <= 213, "95-97" , ifelse (
        db$score <= 236, "90-94" , ifelse (
          db$score <= 250, "82-89" , ifelse (
            db$score <= 271, "72-81" , ifelse (
              db$score <= 302, "60-71" , ifelse (
                db$score <= 406, "41-59" , ifelse (
                  db$score <= 498, "29-40" , ifelse (
                    db$score <= 570, "19-28" , ifelse (
                      db$score <= 599, "11-18" , ifelse (
                        db$score <= 708, "6-10" , ifelse (
                          db$score <= 872, "3-5" , ifelse (
                            db$score <= 873, "2" , ifelse (
                              
                              db$score >= 874, "<1" , NA )))))))))))))))) )
    
    
  }
  
  
  # Educational level adjust 
  db$TOL_ET_education_years_adj <- with(db, ifelse(
    is.na(db$TOL_ET_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years <= 5, db$TOL_ET_scale_score + 1, ifelse(
      db$education_years >= 6  & db$education_years <= 12, db$TOL_ET_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 18, db$TOL_ET_scale_score - 1, ifelse(
          db$education_years >= 19  & db$education_years <= 20, db$TOL_ET_scale_score - 2, ifelse(
            
            )))))) )
  
  
  # NSSae
  db$TOL_ET_NSSae <- with(db, ifelse(
    is.na(db$TOL_ET_education_years_adj), NA, ifelse (
      !is.na(db$TOL_ET_education_years_adj), db$TOL_ET_scale_score - (0.15080*(db$TOL_ET_education_years_adj-12))  )))
  
  return(db)
}
