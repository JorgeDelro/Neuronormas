# Rey-Osterrieth complex figure (ROCF) Copy Time
#
# Reference:
# Peña-Casanova, J., Gramunt-Fombuena, N., Quiñones-Ubeda, M., et al., 2009. Neuronorma
# study team. Spanish Multicenter Normative Studies (NEURONORMA Project): Norms for the Rey–Osterrieth Complex 
# Figure (Copy and Memory), and Free and Cued Selective Reminding Test
# Arch. Clin. Neuropsychol. 24 (4), 371–393.
# 
# 

ROCF_C_TIME <- function(score, age, education_years){
  
  ROCF_C_TIME_db <- data.frame(score = score, age = age, education_years = education_years)
  ROCF_C_TIME_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(ROCF_C_TIME_db)) {
    res <- ROCF_C_TIME_scale_score(score = ROCF_C_TIME_db[i, "score"], 
                              age = ROCF_C_TIME_db[i, "age"],
                              education_years = ROCF_C_TIME_db[i, "education_years"])
    ROCF_C_TIME_new <- rbind(ROCF_C_TIME_new, res)
  }
  
  return(ROCF_C_TIME_new[,c("ROCF_C_TIME_scale_score", "ROCF_C_TIME_percentil_range", "ROCF_C_TIME_NSSae")])
}

ROCF_C_TIME_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 60, 18, ifelse (
        db$score <= 61, 17, ifelse (
          db$score <= 71, 16, ifelse (
            db$score <= 89, 15, ifelse (
              db$score <= 100, 14, ifelse (
                db$score <= 119, 13, ifelse (
                  db$score <= 137, 12, ifelse (
                    db$score <= 154, 11, ifelse (
                      db$score <= 189, 10, ifelse (
                        db$score <= 228, 9, ifelse (
                          db$score <= 262, 8, ifelse (
                            db$score <= 309, 7, ifelse (
                              db$score <= 364, 6, ifelse (
                                db$score <= 409, 5, ifelse (
                                  db$score <= 419, 4, ifelse (
                                    
                                    db$score >= 420, 2, NA ))))))))))))))))) )


# percentile score

db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 60, "> 99" , ifelse (
    db$score <= 61, "99" , ifelse (
      db$score <= 71, "98" , ifelse (
        db$score <= 89, "95-97" , ifelse (
          db$score <= 100, "90-94" , ifelse (
            db$score <= 119, "82-89" , ifelse (
              db$score <= 137, "72-81" , ifelse (
                db$score <= 154, "60-71" , ifelse (
                  db$score <= 189, "41-59" , ifelse (
                    db$score <= 228, "29-40" , ifelse (
                      db$score <= 262, "19-28" , ifelse (
                        db$score <= 309, "11-18" , ifelse (
                          db$score <= 364, "6-10" , ifelse (
                            db$score <= 409, "3-5" , ifelse (
                              db$score <= 419, "2" , ifelse (
                                
                                db$score >= 420, "<1" , NA ))))))))))))))))) )
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 60, 18, ifelse (
        db$score <= 71, 17, ifelse (
          db$score <= 81, 16, ifelse (
            db$score <= 91, 15, ifelse (
              db$score <= 103, 14, ifelse (
                db$score <= 132, 13, ifelse (
                  db$score <= 151, 12, ifelse (
                    db$score <= 174, 11, ifelse (
                      db$score <= 202, 10, ifelse (
                        db$score <= 236, 9, ifelse (
                          db$score <= 284, 8, ifelse (
                            db$score <= 315, 7, ifelse (
                              db$score <= 382, 6, ifelse (
                                db$score <= 419, 5, ifelse (
                                  
                                  db$score <= 422, 3, ifelse (
                                    db$score >= 423, 2, NA ))))))))))))))))) )

# percentile score

db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 60, "> 99" , ifelse (
    db$score <= 71, "99" , ifelse (
      db$score <= 81, "98" , ifelse (
        db$score <= 91, "95-97" , ifelse (
          db$score <= 103, "90-94" , ifelse (
            db$score <= 132, "82-89" , ifelse (
              db$score <= 151, "72-81" , ifelse (
                db$score <= 174, "60-71" , ifelse (
                  db$score <= 202, "41-59" , ifelse (
                    db$score <= 236, "29-40" , ifelse (
                      db$score <= 284, "19-28" , ifelse (
                        db$score <= 315, "11-18" , ifelse (
                          db$score <= 382, "6-10" , ifelse (
                            db$score <= 419, "3-5" , ifelse (
                              
                              db$score <= 422, "1" , ifelse (
                                db$score >= 423, "<1" , NA ))))))))))))))))) )
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 81, 18, ifelse (
        db$score <= 82, 17, ifelse (
          db$score <= 89, 16, ifelse (
            db$score <= 92, 15, ifelse (
              db$score <= 107, 14, ifelse (
                db$score <= 128, 13, ifelse (
                  db$score <= 141, 12, ifelse (
                    db$score <= 164, 11, ifelse (
                      db$score <= 191, 10, ifelse (
                        db$score <= 239, 9, ifelse (
                          db$score <= 294, 8, ifelse (
                            db$score <= 359, 7, ifelse (
                              db$score <= 384, 6, ifelse (
                                db$score <= 419, 5, ifelse (
                                  db$score <= 422, 4, ifelse (
                                    db$score <= 455, 3, ifelse (
                                      db$score >= 456, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 81, "> 99" , ifelse (
        db$score <= 82, "99" , ifelse (
          db$score <= 89, "98" , ifelse (
            db$score <= 92, "95-97" , ifelse (
              db$score <= 107, "90-94" , ifelse (
                db$score <= 128, "82-89" , ifelse (
                  db$score <= 141, "72-81" , ifelse (
                    db$score <= 164, "60-71" , ifelse (
                      db$score <= 191, "41-59" , ifelse (
                        db$score <= 239, "29-40" , ifelse (
                          db$score <= 294, "19-28" , ifelse (
                            db$score <= 359, "11-18" , ifelse (
                              db$score <= 384, "6-10" , ifelse (
                                db$score <= 419, "3-5" , ifelse (
                                  db$score <= 422, "2" , ifelse (
                                    db$score <= 455, "1" , ifelse (
                                      db$score >= 456, "<1" , NA ))))))))))))))))))  )
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 89, 18, ifelse (
        db$score <= 91, 17, ifelse (
          db$score <= 92, 16, ifelse (
            db$score <= 107, 15, ifelse (
              db$score <= 119, 14, ifelse (
                db$score <= 133, 13, ifelse (
                  db$score <= 147, 12, ifelse (
                    db$score <= 172, 11, ifelse (
                      db$score <= 196, 10, ifelse (
                        db$score <= 239, 9, ifelse (
                          db$score <= 289, 8, ifelse (
                            db$score <= 332, 7, ifelse (
                              db$score <= 371, 6, ifelse (
                                db$score <= 419, 5, ifelse (
                                  db$score <= 422, 4, ifelse (
                                    db$score <= 455, 3, ifelse (
                                      db$score >= 456, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 89, "> 99" , ifelse (
        db$score <= 91, "99" , ifelse (
          db$score <= 92, "98" , ifelse (
            db$score <= 107, "95-97" , ifelse (
              db$score <= 119, "90-94" , ifelse (
                db$score <= 133, "82-89" , ifelse (
                  db$score <= 147, "72-81" , ifelse (
                    db$score <= 172, "60-71" , ifelse (
                      db$score <= 196, "41-59" , ifelse (
                        db$score <= 239, "29-40" , ifelse (
                          db$score <= 289, "19-28" , ifelse (
                            db$score <= 332, "11-18" , ifelse (
                              db$score <= 371, "6-10" , ifelse (
                                db$score <= 419, "3-5" , ifelse (
                                  db$score <= 422, "2" , ifelse (
                                    db$score <= 455, "1" , ifelse (
                                      db$score >= 456, "<1" , NA )))))))))))))))))) )
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 75, 18, ifelse (
        db$score <= 89, 17, ifelse (
          db$score <= 91, 16, ifelse (
            db$score <= 104, 15, ifelse (
              db$score <= 118, 14, ifelse (
                db$score <= 135, 13, ifelse (
                  db$score <= 149, 12, ifelse (
                    db$score <= 173, 11, ifelse (
                      db$score <= 214, 10, ifelse (
                        db$score <= 246, 9, ifelse (
                          db$score <= 299, 8, ifelse (
                            db$score <= 371, 7, ifelse (
                              db$score <= 419, 6, ifelse (
                                db$score <= 468, 5, ifelse (
                                  db$score <= 534, 4, ifelse (
                                    db$score <= 544, 3, ifelse (
                                      db$score >= 545, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 75, "> 99" , ifelse (
        db$score <= 89, "99" , ifelse (
          db$score <= 91, "98" , ifelse (
            db$score <= 104, "95-97" , ifelse (
              db$score <= 118, "90-94" , ifelse (
                db$score <= 135, "82-89" , ifelse (
                  db$score <= 149, "72-81" , ifelse (
                    db$score <= 173, "60-71" , ifelse (
                      db$score <= 214, "41-59" , ifelse (
                        db$score <= 246, "29-40" , ifelse (
                          db$score <= 299, "19-28" , ifelse (
                            db$score <= 371, "11-18" , ifelse (
                              db$score <= 419, "6-10" , ifelse (
                                db$score <= 468, "3-5" , ifelse (
                                  db$score <= 534, "2" , ifelse (
                                    db$score <= 544, "1" , ifelse (
                                      db$score >= 545, "<1" , NA )))))))))))))))))) )
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 63, 18, ifelse (
        db$score <= 75, 17, ifelse (
          db$score <= 89, 16, ifelse (
            db$score <= 104, 15, ifelse (
              db$score <= 118, 14, ifelse (
                db$score <= 135, 13, ifelse (
                  db$score <= 151, 12, ifelse (
                    db$score <= 180, 11, ifelse (
                      db$score <= 221, 10, ifelse (
                        db$score <= 253, 9, ifelse (
                          db$score <= 319, 8, ifelse (
                            db$score <= 368, 7, ifelse (
                              db$score <= 412, 6, ifelse (
                                db$score <= 534, 5, ifelse (
                                  db$score <= 544, 4, ifelse (
                                    db$score <= 543, 3, ifelse (
                                      db$score >= 564, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 63, "> 99" , ifelse (
        db$score <= 75, "99" , ifelse (
          db$score <= 89, "98" , ifelse (
            db$score <= 104, "95-97" , ifelse (
              db$score <= 118, "90-94" , ifelse (
                db$score <= 135, "82-89" , ifelse (
                  db$score <= 151, "72-81" , ifelse (
                    db$score <= 180, "60-71" , ifelse (
                      db$score <= 221, "41-59" , ifelse (
                        db$score <= 253, "29-40" , ifelse (
                          db$score <= 319, "19-28" , ifelse (
                            db$score <= 368, "11-18" , ifelse (
                              db$score <= 412, "6-10" , ifelse (
                                db$score <= 534, "3-5" , ifelse (
                                  db$score <= 544, "2" , ifelse (
                                    db$score <= 543, "1" , ifelse (
                                      db$score >= 564, "<1" , NA )))))))))))))))))) )
        
    
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 62, 18, ifelse (
        db$score <= 76, 17, ifelse (
          
          db$score <= 109, 15, ifelse (
            db$score <= 132, 14, ifelse (
              db$score <= 147, 13, ifelse (
                db$score <= 168, 12, ifelse (
                  db$score <= 195, 11, ifelse (
                    db$score <= 246, 10, ifelse (
                      db$score <= 289, 9, ifelse (
                        db$score <= 339, 8, ifelse (
                          db$score <= 382, 7, ifelse (
                            db$score <= 419, 6, ifelse (
                              db$score <= 544, 5, ifelse (
                                db$score <= 563, 4, ifelse (
                                  db$score <= 635, 3, ifelse (
                                    db$score >= 636, 2, NA ))))))))))))))))) )

# percentile score

db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 62, "> 99" , ifelse (
    db$score <= 76, "99" , ifelse (
      
      db$score <= 109, "95-97" , ifelse (
        db$score <= 132, "90-94" , ifelse (
          db$score <= 147, "82-89" , ifelse (
            db$score <= 168, "72-81" , ifelse (
              db$score <= 195, "60-71" , ifelse (
                db$score <= 246, "41-59" , ifelse (
                  db$score <= 289, "29-40" , ifelse (
                    db$score <= 339, "19-28" , ifelse (
                      db$score <= 382, "11-18" , ifelse (
                        db$score <= 419, "6-10" , ifelse (
                          db$score <= 544, "3-5" , ifelse (
                            db$score <= 563, "2" , ifelse (
                              db$score <= 635, "1" , ifelse (
                                db$score >= 636, "<1" , NA ))))))))))))))))) )
    
   
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 61, 18, ifelse (
        db$score <= 75, 17, ifelse (
          db$score <= 89, 16, ifelse (
            db$score <= 109, 15, ifelse (
              db$score <= 119, 14, ifelse (
                db$score <= 151, 13, ifelse (
                  db$score <= 180, 12, ifelse (
                    db$score <= 214, 11, ifelse (
                      db$score <= 275, 10, ifelse (
                        db$score <= 299, 9, ifelse (
                          db$score <= 341, 8, ifelse (
                            db$score <= 405, 7, ifelse (
                              db$score <= 468, 6, ifelse (
                                db$score <= 563, 5, ifelse (
                                  db$score <= 635, 4, ifelse (
                                    db$score <= 636, 3, ifelse (
                                      db$score >= 637, 2, NA )))))))))))))))))) )
    
    # percentile score
    
    db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 61, "> 99" , ifelse (
        db$score <= 75, "99" , ifelse (
          db$score <= 89, "98" , ifelse (
            db$score <= 109, "95-97" , ifelse (
              db$score <= 119, "90-94" , ifelse (
                db$score <= 151, "82-89" , ifelse (
                  db$score <= 180, "72-81" , ifelse (
                    db$score <= 214, "60-71" , ifelse (
                      db$score <= 275, "41-59" , ifelse (
                        db$score <= 299, "29-40" , ifelse (
                          db$score <= 341, "19-28" , ifelse (
                            db$score <= 405, "11-18" , ifelse (
                              db$score <= 468, "6-10" , ifelse (
                                db$score <= 563, "3-5" , ifelse (
                                  db$score <= 635, "2" , ifelse (
                                    db$score <= 636, "1" , ifelse (
                                      db$score >= 637, "<1" , NA ))))))))))))))))))  )
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 89, 18, ifelse (
        db$score <= 99, 17, ifelse (
          
          db$score <= 114, 15, ifelse (
            db$score <= 133, 14, ifelse (
              db$score <= 168, 13, ifelse (
                db$score <= 193, 12, ifelse (
                  db$score <= 220, 11, ifelse (
                    db$score <= 279, 10, ifelse (
                      db$score <= 311, 9, ifelse (
                        db$score <= 329, 8, ifelse (
                          db$score <= 357, 7, ifelse (
                            db$score <= 419, 6, ifelse (
                              db$score <= 563, 5, ifelse (
                                db$score <= 635, 4, ifelse (
                                  
                                  db$score >= 636, 2, NA )))))))))))))))) )

# percentile score

db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 89, "> 99" , ifelse (
    db$score <= 99, "99" , ifelse (
      
      db$score <= 114, "95-97" , ifelse (
        db$score <= 133, "90-94" , ifelse (
          db$score <= 168, "82-89" , ifelse (
            db$score <= 193, "72-81" , ifelse (
              db$score <= 220, "60-71" , ifelse (
                db$score <= 279, "41-59" , ifelse (
                  db$score <= 311, "29-40" , ifelse (
                    db$score <= 329, "19-28" , ifelse (
                      db$score <= 357, "11-18" , ifelse (
                        db$score <= 419, "6-10" , ifelse (
                          db$score <= 563, "3-5" , ifelse (
                            db$score <= 635, "2" , ifelse (
                              
                              db$score >= 636, "<1" , NA ))))))))))))))))  )
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$ROCF_C_TIME_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score <= 89, 18, ifelse (
        
        db$score <= 112, 16, ifelse (
          db$score <= 119, 15, ifelse (
            db$score <= 143, 14, ifelse (
              db$score <= 179, 13, ifelse (
                db$score <= 204, 12, ifelse (
                  db$score <= 255, 11, ifelse (
                    db$score <= 299, 10, ifelse (
                      db$score <= 322, 9, ifelse (
                        db$score <= 349, 8, ifelse (
                          db$score <= 419, 7, ifelse (
                            db$score <= 480, 6, ifelse (
                              db$score <= 564, 5, ifelse (
                                db$score <= 635, 4, ifelse (
                                  
                                  db$score >= 636, 2, NA )))))))))))))))) )

# percentile score

db$ROCF_C_TIME_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score <= 89, "> 99" , ifelse (
    
    db$score <= 112, "98" , ifelse (
      db$score <= 119, "95-97" , ifelse (
        db$score <= 143, "90-94" , ifelse (
          db$score <= 179, "82-89" , ifelse (
            db$score <= 204, "72-81" , ifelse (
              db$score <= 255, "60-71" , ifelse (
                db$score <= 299, "41-59" , ifelse (
                  db$score <= 322, "29-40" , ifelse (
                    db$score <= 349, "19-28" , ifelse (
                      db$score <= 419, "11-18" , ifelse (
                        db$score <= 480, "6-10" , ifelse (
                          db$score <= 564, "3-5" , ifelse (
                            db$score <= 635, "2" , ifelse (
                              
                              db$score >= 636, "<1" , NA ))))))))))))))))  )
    
    
  }
  
  
  # Educational level adjust 
  db$ROCF_C_TIME_education_years_adj <- with(db, ifelse(
    is.na(db$ROCF_C_TIME_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years <= 1, db$ROCF_C_TIME_scale_score + 2, ifelse(
    db$education_years >= 2  & db$education_years <= 6, db$ROCF_C_TIME_scale_score + 1, ifelse(
      db$education_years >= 7  & db$education_years <= 12, db$ROCF_C_TIME_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 17, db$ROCF_C_TIME_scale_score - 1, ifelse(
          db$education_years >= 18  & db$education_years <= 20, db$ROCF_C_TIME_scale_score - 2, ifelse(
            
            ))))))) )
  
  
  # NSSae
  db$ROCF_C_TIME_NSSae <- with(db, ifelse(
    is.na(db$ROCF_C_TIME_education_years_adj), NA, ifelse (
      !is.na(db$ROCF_C_TIME_education_years_adj), db$ROCF_C_TIME_scale_score - (0.19735*(db$ROCF_C_TIME_education_years_adj-12)) )))
  
  return(db)
}
