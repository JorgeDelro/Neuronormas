# Tower of London - Total problem-solving time score

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

# 
TOL_PT <- function(score, age, education_years){
  
  TOL_PT_db <- data.frame(score = score, age = age, education_years = education_years)
  TOL_PT_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(TOL_PT_db)) {
    res <- TOL_PT_scale_score(score = TOL_PT_db[i, "score"], 
                              age = TOL_PT_db[i, "age"],
                              education_years = TOL_PT_db[i, "education_years"])
    TOL_PT_new <- rbind(TOL_PT_new, res)
  }
  
  return(TOL_PT_new[,c("TOL_PT_scale_score", "TOL_PT_percentil_range", "TOL_PT_NSSae")])
}

TOL_PT_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 109, 18, ifelse (
        db$score <= 126, 17, ifelse (
          db$score <= 146, 16, ifelse (
            db$score <= 170, 15, ifelse (
              db$score <= 189, 14, ifelse (
                db$score <= 205, 13, ifelse (
                  db$score <= 239, 12, ifelse (
                    db$score <= 275, 11, ifelse (
                      db$score <= 333, 10, ifelse (
                        db$score <= 367, 9, ifelse (
                          db$score <= 413, 8, ifelse (
                            db$score <= 480, 7, ifelse (
                              db$score <= 546, 6, ifelse (
                                db$score <= 676, 5, ifelse (
                                  db$score <= 709, 4, ifelse (
                                    db$score <= 729, 3, ifelse (
                                      db$score >= 730, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 109, "> 99" , ifelse (
        db$score <= 126, "99" , ifelse (
          db$score <= 146, "98" , ifelse (
            db$score <= 170, "95-97" , ifelse (
              db$score <= 189, "90-94" , ifelse (
                db$score <= 205, "82-89" , ifelse (
                  db$score <= 239, "72-81" , ifelse (
                    db$score <= 275, "60-71" , ifelse (
                      db$score <= 333, "41-59" , ifelse (
                        db$score <= 367, "29-40" , ifelse (
                          db$score <= 413, "19-28" , ifelse (
                            db$score <= 480, "11-18" , ifelse (
                              db$score <= 546, "6-10" , ifelse (
                                db$score <= 676, "3-5" , ifelse (
                                  db$score <= 709, "2" , ifelse (
                                    db$score <= 729, "1" , ifelse (
                                      db$score >= 730, "<1" , NA )))))))))))))))))) )
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 146, 18, ifelse (
        db$score <= 181, 17, ifelse (
          db$score <= 186, 16, ifelse (
            db$score <= 191, 15, ifelse (
              db$score <= 201, 14, ifelse (
                db$score <= 225, 13, ifelse (
                  db$score <= 255, 12, ifelse (
                    db$score <= 282, 11, ifelse (
                      db$score <= 328, 10, ifelse (
                        db$score <= 372, 9, ifelse (
                          db$score <= 418, 8, ifelse (
                            db$score <= 512, 7, ifelse (
                              db$score <= 551, 6, ifelse (
                                db$score <= 676, 5, ifelse (
                                  db$score <= 694, 4, ifelse (
                                    db$score <= 729, 3, ifelse (
                                      db$score >= 730, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 146, "> 99" , ifelse (
        db$score <= 181, "99" , ifelse (
          db$score <= 186, "98" , ifelse (
            db$score <= 191, "95-97" , ifelse (
              db$score <= 201, "90-94" , ifelse (
                db$score <= 225, "82-89" , ifelse (
                  db$score <= 255, "72-81" , ifelse (
                    db$score <= 282, "60-71" , ifelse (
                      db$score <= 328, "41-59" , ifelse (
                        db$score <= 372, "29-40" , ifelse (
                          db$score <= 418, "19-28" , ifelse (
                            db$score <= 512, "11-18" , ifelse (
                              db$score <= 551, "6-10" , ifelse (
                                db$score <= 676, "3-5" , ifelse (
                                  db$score <= 694, "2" , ifelse (
                                    db$score <= 729, "1" , ifelse (
                                      db$score >= 730, "<1" , NA )))))))))))))))))) )
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 146, 18, ifelse (
        db$score <= 153, 17, ifelse (
          db$score <= 171, 16, ifelse (
            db$score <= 189, 15, ifelse (
              db$score <= 201, 14, ifelse (
                db$score <= 212, 13, ifelse (
                  db$score <= 255, 12, ifelse (
                    db$score <= 285, 11, ifelse (
                      db$score <= 330, 10, ifelse (
                        db$score <= 377, 9, ifelse (
                          db$score <= 444, 8, ifelse (
                            db$score <= 528, 7, ifelse (
                              db$score <= 562, 6, ifelse (
                                db$score <= 694, 5, ifelse (
                                  db$score <= 729, 4, ifelse (
                                    db$score <= 783, 3, ifelse (
                                      db$score >= 784, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 146, "> 99" , ifelse (
        db$score <= 153, "99" , ifelse (
          db$score <= 171, "98" , ifelse (
            db$score <= 189, "95-97" , ifelse (
              db$score <= 201, "90-94" , ifelse (
                db$score <= 212, "82-89" , ifelse (
                  db$score <= 255, "72-81" , ifelse (
                    db$score <= 285, "60-71" , ifelse (
                      db$score <= 330, "41-59" , ifelse (
                        db$score <= 377, "29-40" , ifelse (
                          db$score <= 444, "19-28" , ifelse (
                            db$score <= 528, "11-18" , ifelse (
                              db$score <= 562, "6-10" , ifelse (
                                db$score <= 694, "3-5" , ifelse (
                                  db$score <= 729, "2" , ifelse (
                                    db$score <= 783, "1" , ifelse (
                                      db$score >= 784, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 146, 18, ifelse (
        db$score <= 153, 17, ifelse (
          db$score <= 171, 16, ifelse (
            db$score <= 190, 15, ifelse (
              db$score <= 206, 14, ifelse (
                db$score <= 243, 13, ifelse (
                  db$score <= 275, 12, ifelse (
                    db$score <= 307, 11, ifelse (
                      db$score <= 343, 10, ifelse (
                        db$score <= 412, 9, ifelse (
                          db$score <= 458, 8, ifelse (
                            db$score <= 537, 7, ifelse (
                              db$score <= 564, 6, ifelse (
                                db$score <= 729, 5, ifelse (
                                  db$score <= 783, 4, ifelse (
                                    db$score <= 837, 3, ifelse (
                                      db$score >= 838, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 146, "> 99" , ifelse (
        db$score <= 153, "99" , ifelse (
          db$score <= 171, "98" , ifelse (
            db$score <= 190, "95-97" , ifelse (
              db$score <= 206, "90-94" , ifelse (
                db$score <= 243, "82-89" , ifelse (
                  db$score <= 275, "72-81" , ifelse (
                    db$score <= 307, "60-71" , ifelse (
                      db$score <= 343, "41-59" , ifelse (
                        db$score <= 412, "29-40" , ifelse (
                          db$score <= 458, "19-28" , ifelse (
                            db$score <= 537, "11-18" , ifelse (
                              db$score <= 564, "6-10" , ifelse (
                                db$score <= 729, "3-5" , ifelse (
                                  db$score <= 783, "2" , ifelse (
                                    db$score <= 837, "1" , ifelse (
                                      db$score >= 838, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 153, 18, ifelse (
        db$score <= 171, 17, ifelse (
          db$score <= 183, 16, ifelse (
            db$score <= 190, 15, ifelse (
              db$score <= 204, 14, ifelse (
                db$score <= 241, 13, ifelse (
                  db$score <= 266, 12, ifelse (
                    db$score <= 318, 11, ifelse (
                      db$score <= 376, 10, ifelse (
                        db$score <= 433, 9, ifelse (
                          db$score <= 478, 8, ifelse (
                            db$score <= 558, 7, ifelse (
                              db$score <= 629, 6, ifelse (
                                db$score <= 783, 5, ifelse (
                                  db$score <= 831, 4, ifelse (
                                    db$score <= 837, 3, ifelse (
                                      db$score >= 838, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 153, "> 99" , ifelse (
        db$score <= 171, "99" , ifelse (
          db$score <= 183, "98" , ifelse (
            db$score <= 190, "95-97" , ifelse (
              db$score <= 204, "90-94" , ifelse (
                db$score <= 241, "82-89" , ifelse (
                  db$score <= 266, "72-81" , ifelse (
                    db$score <= 318, "60-71" , ifelse (
                      db$score <= 376, "41-59" , ifelse (
                        db$score <= 433, "29-40" , ifelse (
                          db$score <= 478, "19-28" , ifelse (
                            db$score <= 558, "11-18" , ifelse (
                              db$score <= 629, "6-10" , ifelse (
                                db$score <= 783, "3-5" , ifelse (
                                  db$score <= 831, "2" , ifelse (
                                    db$score <= 837, "1" , ifelse (
                                      db$score >= 838, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 153, 18, ifelse (
        db$score <= 171, 17, ifelse (
          db$score <= 183, 16, ifelse (
            db$score <= 190, 15, ifelse (
              db$score <= 204, 14, ifelse (
                db$score <= 241, 13, ifelse (
                  db$score <= 276, 12, ifelse (
                    db$score <= 328, 11, ifelse (
                      db$score <= 406, 10, ifelse (
                        db$score <= 458, 9, ifelse (
                          db$score <= 540, 8, ifelse (
                            db$score <= 590, 7, ifelse (
                              db$score <= 735, 6, ifelse (
                                db$score <= 831, 5, ifelse (
                                  db$score <= 837, 4, ifelse (
                                    db$score <= 875, 3, ifelse (
                                      db$score >= 876, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 153, "> 99" , ifelse (
        db$score <= 171, "99" , ifelse (
          db$score <= 183, "98" , ifelse (
            db$score <= 190, "95-97" , ifelse (
              db$score <= 204, "90-94" , ifelse (
                db$score <= 241, "82-89" , ifelse (
                  db$score <= 276, "72-81" , ifelse (
                    db$score <= 328, "60-71" , ifelse (
                      db$score <= 406, "41-59" , ifelse (
                        db$score <= 458, "29-40" , ifelse (
                          db$score <= 540, "19-28" , ifelse (
                            db$score <= 590, "11-18" , ifelse (
                              db$score <= 735, "6-10" , ifelse (
                                db$score <= 831, "3-5" , ifelse (
                                  db$score <= 837, "2" , ifelse (
                                    db$score <= 875, "1" , ifelse (
                                      db$score >= 876, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <=  185, 18, ifelse (
        db$score <= 188, 17, ifelse (
          db$score <= 191, 16, ifelse (
            db$score <= 202, 15, ifelse (
              db$score <= 229, 14, ifelse (
                db$score <= 256, 13, ifelse (
                  db$score <= 297, 12, ifelse (
                    db$score <= 334, 11, ifelse (
                      db$score <= 432, 10, ifelse (
                        db$score <= 480, 9, ifelse (
                          db$score <= 561, 8, ifelse (
                            db$score <= 640, 7, ifelse (
                              db$score <= 735, 6, ifelse (
                                db$score <= 831, 5, ifelse (
                                  db$score <= 837, 4, ifelse (
                                    db$score <= 875, 3, ifelse (
                                      db$score >=  876, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <=  185, "> 99" , ifelse (
        db$score <= 188, "99" , ifelse (
          db$score <= 191, "98" , ifelse (
            db$score <= 202, "95-97" , ifelse (
              db$score <= 229, "90-94" , ifelse (
                db$score <= 256, "82-89" , ifelse (
                  db$score <= 297, "72-81" , ifelse (
                    db$score <= 334, "60-71" , ifelse (
                      db$score <= 432, "41-59" , ifelse (
                        db$score <= 480, "29-40" , ifelse (
                          db$score <= 561, "19-28" , ifelse (
                            db$score <= 640, "11-18" , ifelse (
                              db$score <= 735, "6-10" , ifelse (
                                db$score <= 831, "3-5" , ifelse (
                                  db$score <= 837, "2" , ifelse (
                                    db$score <= 875, "1" , ifelse (
                                      db$score >=  876, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 184, 18, ifelse (
        db$score <= 185, 17, ifelse (
          db$score <= 195, 16, ifelse (
            db$score <= 215, 15, ifelse (
              db$score <= 238, 14, ifelse (
                db$score <= 260, 13, ifelse (
                  db$score <= 310, 12, ifelse (
                    db$score <= 353, 11, ifelse (
                      db$score <= 442, 10, ifelse (
                        db$score <= 497, 9, ifelse (
                          db$score <= 578, 8, ifelse (
                            db$score <= 640, 7, ifelse (
                              db$score <= 735, 6, ifelse (
                                db$score <= 766, 5, ifelse (
                                  db$score <= 831, 4, ifelse (
                                    db$score <= 875, 3, ifelse (
                                      db$score >= 876, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$TOL_PT_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 184, "> 99" , ifelse (
        db$score <= 185, "99" , ifelse (
          db$score <= 195, "98" , ifelse (
            db$score <= 215, "95-97" , ifelse (
              db$score <= 238, "90-94" , ifelse (
                db$score <= 260, "82-89" , ifelse (
                  db$score <= 310, "72-81" , ifelse (
                    db$score <= 353, "60-71" , ifelse (
                      db$score <= 442, "41-59" , ifelse (
                        db$score <= 497, "29-40" , ifelse (
                          db$score <= 578, "19-28" , ifelse (
                            db$score <= 640, "11-18" , ifelse (
                              db$score <= 735, "6-10" , ifelse (
                                db$score <= 766, "3-5" , ifelse (
                                  db$score <= 831, "2" , ifelse (
                                    db$score <= 875, "1" , ifelse (
                                      db$score >= 876, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 185, 18, ifelse (
        
        db$score <= 217, 16, ifelse (
          db$score <= 229, 15, ifelse (
            db$score <= 255, 14, ifelse (
              db$score <= 305, 13, ifelse (
                db$score <= 329, 12, ifelse (
                  db$score <= 391, 11, ifelse (
                    db$score <= 480, 10, ifelse (
                      db$score <= 540, 9, ifelse (
                        db$score <= 600, 8, ifelse (
                          db$score <= 661, 7, ifelse (
                            db$score <= 735, 6, ifelse (
                              db$score <= 875, 5, ifelse (
                                
                                db$score <= 876, 3, ifelse (
                                  db$score >= 877, 2, NA )))))))))))))))) )

# percentile score

db$TOL_PT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 185, "> 99" , ifelse (
    
    db$score <= 217, "98" , ifelse (
      db$score <= 229, "95-97" , ifelse (
        db$score <= 255, "90-94" , ifelse (
          db$score <= 305, "82-89" , ifelse (
            db$score <= 329, "72-81" , ifelse (
              db$score <= 391, "60-71" , ifelse (
                db$score <= 480, "41-59" , ifelse (
                  db$score <= 540, "29-40" , ifelse (
                    db$score <= 600, "19-28" , ifelse (
                      db$score <= 661, "11-18" , ifelse (
                        db$score <= 735, "6-10" , ifelse (
                          db$score <= 875, "3-5" , ifelse (
                            
                            db$score <= 876, "1" , ifelse (
                              db$score >= 877, "<1" , NA )))))))))))))))) )
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    
    db$TOL_PT_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 251, 18, ifelse (
        
        db$score <= 255, 16, ifelse (
          db$score <= 281, 15, ifelse (
            db$score <= 297, 14, ifelse (
              db$score <= 306, 13, ifelse (
                db$score <= 353, 12, ifelse (
                  db$score <= 394, 11, ifelse (
                    db$score <= 473, 10, ifelse (
                      db$score <= 575, 9, ifelse (
                        db$score <= 633, 8, ifelse (
                          db$score <= 710, 7, ifelse (
                            db$score <= 781, 6, ifelse (
                              db$score <= 966, 5, ifelse (
                                db$score <= 967, 4, ifelse (
                                  
                                  db$score >= 968, 2, NA )))))))))))))))) )

# percentile score

db$TOL_PT_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 251, "> 99" , ifelse (
    
    db$score <= 255, "98" , ifelse (
      db$score <= 281, "95-97" , ifelse (
        db$score <= 297, "90-94" , ifelse (
          db$score <= 306, "82-89" , ifelse (
            db$score <= 353, "72-81" , ifelse (
              db$score <= 394, "60-71" , ifelse (
                db$score <= 473, "41-59" , ifelse (
                  db$score <= 575, "29-40" , ifelse (
                    db$score <= 633, "19-28" , ifelse (
                      db$score <= 710, "11-18" , ifelse (
                        db$score <= 781, "6-10" , ifelse (
                          db$score <= 966, "3-5" , ifelse (
                            db$score <= 967, "2" , ifelse (
                              
                              db$score >= 968, "< 1" , NA )))))))))))))))) )
    
  }
  
  
  # Educational level adjust 
  db$TOL_PT_education_years_adj <- with(db, ifelse(
    is.na(db$TOL_PT_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years <= 3, db$TOL_PT_scale_score + 1, ifelse(
      db$education_years >= 4  & db$education_years <= 12, db$TOL_PT_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 20, db$TOL_PT_scale_score - 1, ifelse(
          
            ))))) )
  
  
  # NSSae
  db$TOL_PT_NSSae <- with(db, ifelse(
    is.na(db$TOL_PT_education_years_adj), NA, ifelse (
      !is.na(db$TOL_PT_education_years_adj), db$TOL_PT_scale_score - (0.11423*(db$TOL_PT_education_years_adj-12))  )))
  
  return(db)
}
