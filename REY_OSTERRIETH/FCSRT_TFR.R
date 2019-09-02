# Free and Cued Selective Reminding Test (FCSRT) Total Free Recall

# install.packages("readxl")
library(readxl)

db <- read_xls("COG_BRUTO.xls")



FCSRT_TFR <- FCSRT_TFR_function(score = db$Q_COG_COWAT_FCSRT_TFR_PRE,
                            age = db$AGE_PRE,
                            education_years = db$EDUCATIONAL_LEVEL)

# Function GORDA
FCSRT_TFR_function <- function(score, age, education_years){
  
  FCSRT_TFR <- data.frame(score = score, age = age, education_years = education_years)
  FCSRT_TFR_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(FCSRT_TFR)) {
    res <- FCSRT_TFR_scale_score(score = FCSRT_TFR[i, "score"], 
                              age = FCSRT_TFR[i, "age"],
                              education_years = FCSRT_TFR[i, "education_years"])
    FCSRT_TFR_new <- rbind(FCSRT_TFR_new, res)
  }
  
  return(FCSRT_TFR_new)
}

FCSRT_TFR_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 39 , 18, ifelse (
        
        db$score >= 37 , 16, ifelse (
          
          db$score >= 34 , 14, ifelse (
            db$score >= 33, 13, ifelse (
              db$score >= 31 , 12, ifelse (
                db$score >= 29 , 11, ifelse (
                  db$score >= 26 , 10, ifelse (
                    db$score >= 23 , 9, ifelse (
                      db$score >= 21 , 8, ifelse (
                        db$score >= 19 , 7, ifelse (
                          db$score >= 18, 6, ifelse (
                            db$score >= 14 , 5, ifelse (
                              
                              db$score >= 12 , 3, ifelse (
                                db$score <=  11, 2, NA )))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 39 , "> 99" , ifelse (
    
    db$score >= 37 , "98" , ifelse (
      
      db$score >= 34 , "90-94" , ifelse (
        db$score >= 33, "82-89" , ifelse (
          db$score >= 31 , "72-81" , ifelse (
            db$score >= 29 , "60-71" , ifelse (
              db$score >= 26 , "41-59" , ifelse (
                db$score >= 23 , "29-40" , ifelse (
                  db$score >= 21 , "19-28" , ifelse (
                    db$score >= 19 , "11-18" , ifelse (
                      db$score >= 18, "6-10" , ifelse (
                        db$score >= 14 , "3-5" , ifelse (
                          
                          db$score >= 12 , "1" , ifelse (
                            db$score <=  11, "<1" , NA )))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 40 , 18, ifelse (
        db$score >= 39, 17, ifelse (
          db$score >= 38, 16, ifelse (
            db$score >= 37, 15, ifelse (
              db$score >= 34 , 14, ifelse (
                db$score >= 32 , 13, ifelse (
                  db$score >= 30 , 12, ifelse (
                    db$score >= 28 , 11, ifelse (
                      db$score >= 24 , 10, ifelse (
                        db$score >= 23, 9, ifelse (
                          db$score >= 21 , 8, ifelse (
                            db$score >= 20, 7, ifelse (
                              db$score >= 17 , 6, ifelse (
                                db$score >= 13 , 5, ifelse (
                                  
                                  db$score >= 12, 3, ifelse (
                                    db$score <=  11, 2, NA )))))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 40 , "> 99" , ifelse (
    db$score >= 39, "99" , ifelse (
      db$score >= 38, "98" , ifelse (
        db$score >= 37, "95-97" , ifelse (
          db$score >= 34 , "90-94" , ifelse (
            db$score >= 32 , "82-89" , ifelse (
              db$score >= 30 , "72-81" , ifelse (
                db$score >= 28 , "60-71" , ifelse (
                  db$score >= 24 , "41-59" , ifelse (
                    db$score >= 23, "29-40" , ifelse (
                      db$score >= 21 , "19-28" , ifelse (
                        db$score >= 20, "11-18" , ifelse (
                          db$score >= 17 , "6-10" , ifelse (
                            db$score >= 13 , "3-5" , ifelse (
                              
                              db$score >= 12, "1" , ifelse (
                                db$score <=  11, "<1" , NA )))))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 40 , 18, ifelse (
        db$score >= 39, 17, ifelse (
          db$score >= 38, 16, ifelse (
            db$score >= 37, 15, ifelse (
              db$score >= 34 , 14, ifelse (
                db$score >= 32 , 13, ifelse (
                  db$score >= 30 , 12, ifelse (
                    db$score >= 28 , 11, ifelse (
                      db$score >= 24 , 10, ifelse (
                        db$score >= 22 , 9, ifelse (
                          db$score >= 21, 8, ifelse (
                            db$score >= 19 , 7, ifelse (
                              db$score >= 16 , 6, ifelse (
                                db$score >= 14 , 5, ifelse (
                                  db$score >= 13, 4, ifelse (
                                    db$score >= 12, 3, ifelse (
                                      db$score <=  11, 2, NA ))))))))))))))))))
    
    # percentile score
    
    db$FCSRT_TFR_percentil_range <- with (db, ifelse (
      db$score >= 40 , "> 99" , ifelse (
        db$score >= 39, "99" , ifelse (
          db$score >= 38, "98" , ifelse (
            db$score >= 37, "95-97" , ifelse (
              db$score >= 34 , "90-94" , ifelse (
                db$score >= 32 , "82-89" , ifelse (
                  db$score >= 30 , "72-81" , ifelse (
                    db$score >= 28 , "60-71" , ifelse (
                      db$score >= 24 , "41-59" , ifelse (
                        db$score >= 22 , "29-40" , ifelse (
                          db$score >= 21, "19-28" , ifelse (
                            db$score >= 19 , "11-18" , ifelse (
                              db$score >= 16 , "6-10" , ifelse (
                                db$score >= 14 , "3-5" , ifelse (
                                  db$score >= 13, "2" , ifelse (
                                    db$score >= 12, "1" , ifelse (
                                      db$score <=  11, "<1" , NA ))))))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 39 , 18, ifelse (
        db$score >= 38, 17, ifelse (
          db$score >= 37, 16, ifelse (
            db$score >= 35 , 15, ifelse (
              db$score >= 33 , 14, ifelse (
                db$score >= 31 , 13, ifelse (
                  db$score >= 28 , 12, ifelse (
                    db$score >= 26 , 11, ifelse (
                      db$score >= 23 , 10, ifelse (
                        db$score >= 21 , 9, ifelse (
                          db$score >= 20, 8, ifelse (
                            db$score >= 17 , 7, ifelse (
                              db$score >= 15 , 6, ifelse (
                                db$score >= 13 , 5, ifelse (
                                  
                                  
                                  db$score <=  12, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 39 , "> 99" , ifelse (
    db$score >= 38, "99" , ifelse (
      db$score >= 37, "98" , ifelse (
        db$score >= 35 , "95-97" , ifelse (
          db$score >= 33 , "90-94" , ifelse (
            db$score >= 31 , "82-89" , ifelse (
              db$score >= 28 , "72-81" , ifelse (
                db$score >= 26 , "60-71" , ifelse (
                  db$score >= 23 , "41-59" , ifelse (
                    db$score >= 21 , "29-40" , ifelse (
                      db$score >= 20, "19-28" , ifelse (
                        db$score >= 17 , "11-18" , ifelse (
                          db$score >= 15 , "6-10" , ifelse (
                            db$score >= 13 , "3-5" , ifelse (
                              
                              
                              db$score <=  12, "<1" , NA )))))))))))))))) 
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 35 , 18, ifelse (
        
        db$score >= 34, 16, ifelse (
          db$score >= 32 , 15, ifelse (
            db$score >= 31, 14, ifelse (
              db$score >= 28 , 13, ifelse (
                db$score >= 26 , 12, ifelse (
                  db$score >= 24 , 11, ifelse (
                    db$score >= 22 , 10, ifelse (
                      db$score >= 20 , 9, ifelse (
                        db$score >= 17 , 8, ifelse (
                          db$score >= 15 , 7, ifelse (
                            db$score >= 10 , 6, ifelse (
                              db$score >= 9, 5, ifelse (
                                db$score >= 8, 4, ifelse (
                                  db$score >= 3, 3, ifelse (
                                    db$score <= 2, 2, NA )))))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 35 , "> 99" , ifelse (
    
    db$score >= 34, "98" , ifelse (
      db$score >= 32 , "95-97" , ifelse (
        db$score >= 31, "90-94" , ifelse (
          db$score >= 28 , "82-89" , ifelse (
            db$score >= 26 , "72-81" , ifelse (
              db$score >= 24 , "60-71" , ifelse (
                db$score >= 22 , "41-59" , ifelse (
                  db$score >= 20 , "29-40" , ifelse (
                    db$score >= 17 , "19-28" , ifelse (
                      db$score >= 15 , "11-18" , ifelse (
                        db$score >= 10 , "6-10" , ifelse (
                          db$score >= 9, "3-5" , ifelse (
                            db$score >= 8, "2" , ifelse (
                              db$score >= 3, "1" , ifelse (
                                db$score <= 2, "<1" , NA )))))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 34 , 18, ifelse (
        
        db$score >= 33, 16, ifelse (
          db$score >= 32, 15, ifelse (
            db$score >= 30 , 14, ifelse (
              db$score >= 27 , 13, ifelse (
                db$score >= 26, 12, ifelse (
                  db$score >= 23 , 11, ifelse (
                    db$score >= 20 , 10, ifelse (
                      db$score >= 18 , 9, ifelse (
                        db$score >= 15 , 8, ifelse (
                          db$score >= 13 , 7, ifelse (
                            db$score >= 10 , 6, ifelse (
                              db$score >= 8, 5, ifelse (
                                db$score >= 5, 4, ifelse (
                                  db$score >= 3, 3, ifelse (
                                    db$score <= 2, 2, NA )))))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 34 , "> 99" , ifelse (
    
    db$score >= 33, "98" , ifelse (
      db$score >= 32, "95-97" , ifelse (
        db$score >= 30 , "90-94" , ifelse (
          db$score >= 27 , "82-89" , ifelse (
            db$score >= 26, "72-81" , ifelse (
              db$score >= 23 , "60-71" , ifelse (
                db$score >= 20 , "41-59" , ifelse (
                  db$score >= 18 , "29-40" , ifelse (
                    db$score >= 15 , "19-28" , ifelse (
                      db$score >= 13 , "11-18" , ifelse (
                        db$score >= 10 , "6-10" , ifelse (
                          db$score >= 8, "3-5" , ifelse (
                            db$score >= 5, "2" , ifelse (
                              db$score >= 3, "1" , ifelse (
                                db$score <= 2, "<1" , NA )))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 34 , 18, ifelse (
        
        db$score >= 33, 16, ifelse (
          db$score >= 31 , 15, ifelse (
            db$score >= 29 , 14, ifelse (
              db$score >= 26 , 13, ifelse (
                db$score >= 24 , 12, ifelse (
                  db$score >= 22 , 11, ifelse (
                    db$score >= 19 , 10, ifelse (
                      db$score >= 16 , 9, ifelse (
                        db$score >= 14 , 8, ifelse (
                          db$score >= 13, 7, ifelse (
                            db$score >= 9, 6, ifelse (
                              db$score >= 5, 5, ifelse (
                                
                                db$score >= 3, 3, ifelse (
                                  db$score <= 2, 2, NA ))))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 34 , "> 99" , ifelse (
    
    db$score >= 33, "98" , ifelse (
      db$score >= 31 , "95-97" , ifelse (
        db$score >= 29 , "90-94" , ifelse (
          db$score >= 26 , "82-89" , ifelse (
            db$score >= 24 , "72-81" , ifelse (
              db$score >= 22 , "60-71" , ifelse (
                db$score >= 19 , "41-59" , ifelse (
                  db$score >= 16 , "29-40" , ifelse (
                    db$score >= 14 , "19-28" , ifelse (
                      db$score >= 13, "11-18" , ifelse (
                        db$score >= 9, "6-10" , ifelse (
                          db$score >= 5, "3-5" , ifelse (
                            
                            db$score >= 3, "1" , ifelse (
                              db$score <= 2, "<1" , NA ))))))))))))))))
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 33 , 18, ifelse (
        
        db$score >= 32, 16, ifelse (
          db$score >= 31, 15, ifelse (
            db$score >= 29 , 14, ifelse (
              db$score >= 26 , 13, ifelse (
                db$score >= 23 , 12, ifelse (
                  db$score >= 21 , 11, ifelse (
                    db$score >= 17 , 10, ifelse (
                      db$score >= 15 , 9, ifelse (
                        db$score >= 13 , 8, ifelse (
                          db$score >= 10 , 7, ifelse (
                            db$score >= 6, 6, ifelse (
                              db$score >= 5, 5, ifelse (
                                db$score >= 3, 4, ifelse (
                                  db$score >= 2, 3, ifelse (
                                    db$score <= 1, 2, NA )))))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 33 , "> 99" , ifelse (
    
    db$score >= 32, "98" , ifelse (
      db$score >= 31, "95-97" , ifelse (
        db$score >= 29 , "90-94" , ifelse (
          db$score >= 26 , "82-89" , ifelse (
            db$score >= 23 , "72-81" , ifelse (
              db$score >= 21 , "60-71" , ifelse (
                db$score >= 17 , "41-59" , ifelse (
                  db$score >= 15 , "29-40" , ifelse (
                    db$score >= 13 , "19-28" , ifelse (
                      db$score >= 10 , "11-18" , ifelse (
                        db$score >= 6, "6-10" , ifelse (
                          db$score >= 5, "3-5" , ifelse (
                            db$score >= 3, "2" , ifelse (
                              db$score >= 2, "1" , ifelse (
                                db$score <= 1, "<1" , NA ))))))))))))))))) 
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 32 , 18, ifelse (
        
        db$score >= 31, 16, ifelse (
          
          db$score >= 26 , 14, ifelse (
            db$score >= 22 , 13, ifelse (
              db$score >= 21, 12, ifelse (
                db$score >= 18 , 11, ifelse (
                  db$score >= 15 , 10, ifelse (
                    db$score >= 14, 9, ifelse (
                      db$score >= 13, 8, ifelse (
                        db$score >= 10 , 7, ifelse (
                          db$score >= 8, 6, ifelse (
                            db$score >= 5, 5, ifelse (
                              
                              
                              db$score <= 4, 2, NA ))))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 32 , "> 99" , ifelse (
    
    db$score >= 31, "98" , ifelse (
      
      db$score >= 26 , "90-94" , ifelse (
        db$score >= 22 , "82-89" , ifelse (
          db$score >= 21, "72-81" , ifelse (
            db$score >= 18 , "60-71" , ifelse (
              db$score >= 15 , "41-59" , ifelse (
                db$score >= 14, "29-40" , ifelse (
                  db$score >= 13, "19-28" , ifelse (
                    db$score >= 10 , "11-18" , ifelse (
                      db$score >= 8, "6-10" , ifelse (
                        db$score >= 5, "3-5" , ifelse (
                          
                          
                          db$score <= 4, "<1" , NA ))))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$FCSRT_TFR_scale_score <- with ( db, ifelse (
      db$score >= 31, 18, ifelse (
        
        
        db$score >= 24, 15, ifelse (
          db$score >= 23, 14, ifelse (
            db$score >= 22, 13, ifelse (
              db$score >= 20, 12, ifelse (
                db$score >= 17, 11, ifelse (
                  db$score >= 14, 10, ifelse (
                    
                    db$score >= 12, 8, ifelse (
                      db$score >= 9, 7, ifelse (
                        db$score >= 5, 6, ifelse (
                          
                          
                          
                          db$score <= 4, 2, NA ))))))))))))

# percentile score

db$FCSRT_TFR_percentil_range <- with (db, ifelse (
  db$score >= 31, "> 99" , ifelse (
    
    
    db$score >= 24, "95-97" , ifelse (
      db$score >= 23, "90-94" , ifelse (
        db$score >= 22, "82-89" , ifelse (
          db$score >= 20, "72-81" , ifelse (
            db$score >= 17, "60-71" , ifelse (
              db$score >= 14, "41-59" , ifelse (
                
                db$score >= 12, "19-28" , ifelse (
                  db$score >= 9, "11-18" , ifelse (
                    db$score >= 5, "6-10" , ifelse (
                      
                      
                      
                      db$score <= 4, "<1" , NA ))))))))))))
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$FCSRT_TFR_scale_score + 2, ifelse( 
    db$education_years >= 1  & db$education_years <= 6, db$FCSRT_TFR_scale_score + 1, ifelse(
      db$education_years >= 7  & db$education_years <= 12, db$FCSRT_TFR_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 17, db$FCSRT_TFR_scale_score - 1, ifelse(
          db$education_years >= 18  & db$education_years <= 20, db$FCSRT_TFR_scale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
  db$NSSae_FCSRT_TFR <- db$FCSRT_TFR_scale_score - (0.17127*(db$education_years_adj-12)) 
  
  return(db)
}
