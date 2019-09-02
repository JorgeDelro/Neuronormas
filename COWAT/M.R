# M
# install.packages("readxl")
library(readxl)

db <- read_xls("COG_BRUTO.xls")



m <- m_function(score = db$Q_COG_COWAT_m_PRE,
                            age = db$AGE_PRE,
                            education_years = db$EDUCATIONAL_LEVEL)

# Function GORDA
m_function <- function(score, age, education_years){
  
  m <- data.frame(score = score, age = age, education_years = education_years)
  m_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(m)) {
    res <- mscale_score(score = m[i, "score"], 
                              age = m[i, "age"],
                              education_years = m[i, "education_years"])
    m_new <- rbind(m_new, res)
  }
  
  return(m_new)
}

mscale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        
        db$score >= 23 , 16, ifelse (
          db$score >= 21 , 15, ifelse (
            db$score >= 18 , 14, ifelse (
              db$score >= 17, 13, ifelse (
                db$score >= 15 , 12, ifelse (
                  db$score >= 14, 11, ifelse (
                    db$score >= 12 , 10, ifelse (
                      db$score >= 10 , 9, ifelse (
                        db$score >= 9, 8, ifelse (
                          db$score >= 7, 7, ifelse (
                            db$score >= 6, 6, ifelse (
                              db$score >= 5, 5, ifelse (
                                db$score >= 2, 4, ifelse (
                                  
                                  db$score <= 1, 2, NA ))))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    
    db$score >= 23 , "98" , ifelse (
      db$score >= 21 , "95-97" , ifelse (
        db$score >= 18 , "90-94" , ifelse (
          db$score >= 17, "82-89" , ifelse (
            db$score >= 15 , "72-81" , ifelse (
              db$score >= 14, "60-71" , ifelse (
                db$score >= 12 , "41-59" , ifelse (
                  db$score >= 10 , "29-40" , ifelse (
                    db$score >= 9, "19-28" , ifelse (
                      db$score >= 7, "11-18" , ifelse (
                        db$score >= 6, "6-10" , ifelse (
                          db$score >= 5, "3-5" , ifelse (
                            db$score >= 2, "2" , ifelse (
                              
                              db$score <= 1, "<1" , NA ))))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 24, 17, ifelse (
          db$score >= 23, 16, ifelse (
            db$score >= 21 , 15, ifelse (
              db$score >= 18 , 14, ifelse (
                db$score >= 16 , 13, ifelse (
                  db$score >= 15, 12, ifelse (
                    db$score >= 13 , 11, ifelse (
                      db$score >= 11 , 10, ifelse (
                        db$score >= 10, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 6, 7, ifelse (
                              db$score >= 5, 6, ifelse (
                                db$score >= 4, 5, ifelse (
                                  db$score >= 2, 4, ifelse (
                                    
                                    db$score <= 1, 2, NA )))))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    db$score >= 24, "99" , ifelse (
      db$score >= 23, "98" , ifelse (
        db$score >= 21 , "95-97" , ifelse (
          db$score >= 18 , "90-94" , ifelse (
            db$score >= 16 , "82-89" , ifelse (
              db$score >= 15, "72-81" , ifelse (
                db$score >= 13 , "60-71" , ifelse (
                  db$score >= 11 , "41-59" , ifelse (
                    db$score >= 10, "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 6, "11-18" , ifelse (
                          db$score >= 5, "6-10" , ifelse (
                            db$score >= 4, "3-5" , ifelse (
                              db$score >= 2, "2" , ifelse (
                                
                                db$score <= 1, "<1" , NA ))))))))))))))))) 
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 23 , 17, ifelse (
          db$score >= 21 , 16, ifelse (
            db$score >= 19 , 15, ifelse (
              db$score >= 17 , 14, ifelse (
                db$score >= 16, 13, ifelse (
                  db$score >= 15, 12, ifelse (
                    db$score >= 12 , 11, ifelse (
                      db$score >= 10 , 10, ifelse (
                        db$score >= 9, 9, ifelse (
                          db$score >= 7, 8, ifelse (
                            db$score >= 6, 7, ifelse (
                              db$score >= 5, 6, ifelse (
                                db$score >= 4, 5, ifelse (
                                  
                                  db$score >= 2, 3, ifelse (
                                    db$score <= 1, 2, NA )))))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    db$score >= 23 , "99" , ifelse (
      db$score >= 21 , "98" , ifelse (
        db$score >= 19 , "95-97" , ifelse (
          db$score >= 17 , "90-94" , ifelse (
            db$score >= 16, "82-89" , ifelse (
              db$score >= 15, "72-81" , ifelse (
                db$score >= 12 , "60-71" , ifelse (
                  db$score >= 10 , "41-59" , ifelse (
                    db$score >= 9, "29-40" , ifelse (
                      db$score >= 7, "19-28" , ifelse (
                        db$score >= 6, "11-18" , ifelse (
                          db$score >= 5, "6-10" , ifelse (
                            db$score >= 4, "3-5" , ifelse (
                              
                              db$score >= 2, "1" , ifelse (
                                db$score <= 1, "<1" , NA )))))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        db$score >= 21 , 17, ifelse (
          
          db$score >= 20, 15, ifelse (
            db$score >= 18 , 14, ifelse (
              db$score >= 16 , 13, ifelse (
                db$score >= 15, 12, ifelse (
                  db$score >= 13 , 11, ifelse (
                    db$score >= 10 , 10, ifelse (
                      db$score >= 9, 9, ifelse (
                        db$score >= 7, 8, ifelse (
                          db$score >= 6, 7, ifelse (
                            
                            db$score >= 5, 5, ifelse (
                              db$score >= 4, 4, ifelse (
                                
                                db$score <= 3, 2, NA )))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    db$score >= 21 , "99" , ifelse (
      
      db$score >= 20, "95-97" , ifelse (
        db$score >= 18 , "90-94" , ifelse (
          db$score >= 16 , "82-89" , ifelse (
            db$score >= 15, "72-81" , ifelse (
              db$score >= 13 , "60-71" , ifelse (
                db$score >= 10 , "41-59" , ifelse (
                  db$score >= 9, "29-40" , ifelse (
                    db$score >= 7, "19-28" , ifelse (
                      db$score >= 6, "11-18" , ifelse (
                        
                        db$score >= 5, "3-5" , ifelse (
                          db$score >= 4, "2" , ifelse (
                            
                            db$score <= 3, "<1" , NA )))))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        db$score >= 22, 17, ifelse (
          
          db$score >= 20 , 15, ifelse (
            db$score >= 18 , 14, ifelse (
              db$score >= 16 , 13, ifelse (
                db$score >= 15, 12, ifelse (
                  db$score >= 13 , 11, ifelse (
                    db$score >= 10 , 10, ifelse (
                      db$score >= 9, 9, ifelse (
                        db$score >= 7, 8, ifelse (
                          db$score >= 6, 7, ifelse (
                            db$score >= 5, 6, ifelse (
                              db$score >= 4, 5, ifelse (
                                
                                
                                db$score <= 3, 2, NA )))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    db$score >= 22, "99" , ifelse (
      
      db$score >= 20 , "95-97" , ifelse (
        db$score >= 18 , "90-94" , ifelse (
          db$score >= 16 , "82-89" , ifelse (
            db$score >= 15, "72-81" , ifelse (
              db$score >= 13 , "60-71" , ifelse (
                db$score >= 10 , "41-59" , ifelse (
                  db$score >= 9, "29-40" , ifelse (
                    db$score >= 7, "19-28" , ifelse (
                      db$score >= 6, "11-18" , ifelse (
                        db$score >= 5, "6-10" , ifelse (
                          db$score >= 4, "3-5" , ifelse (
                            
                            
                            db$score <= 3, "<1" , NA )))))))))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        db$score >= 22, 17, ifelse (
          db$score >= 21, 16, ifelse (
            db$score >= 19 , 15, ifelse (
              db$score >= 18, 14, ifelse (
                db$score >= 16 , 13, ifelse (
                  db$score >= 15, 12, ifelse (
                    db$score >= 12 , 11, ifelse (
                      db$score >= 10 , 10, ifelse (
                        db$score >= 8, 9, ifelse (
                          db$score >= 7, 8, ifelse (
                            db$score >= 5, 7, ifelse (
                              db$score >= 4, 6, ifelse (
                                
                                db$score >= 3, 4, ifelse (
                                  
                                  db$score <= 2, 2, NA ))))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    db$score >= 22, "99" , ifelse (
      db$score >= 21, "98" , ifelse (
        db$score >= 19 , "95-97" , ifelse (
          db$score >= 18, "90-94" , ifelse (
            db$score >= 16 , "82-89" , ifelse (
              db$score >= 15, "72-81" , ifelse (
                db$score >= 12 , "60-71" , ifelse (
                  db$score >= 10 , "41-59" , ifelse (
                    db$score >= 8, "29-40" , ifelse (
                      db$score >= 7, "19-28" , ifelse (
                        db$score >= 5, "11-18" , ifelse (
                          db$score >= 4, "6-10" , ifelse (
                            
                            db$score >= 3, "2" , ifelse (
                              
                              db$score <= 2, "<1" , NA ))))))))))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 21, 17, ifelse (
          
          db$score >= 18 , 15, ifelse (
            
            db$score >= 16 , 13, ifelse (
              db$score >= 13 , 12, ifelse (
                db$score >= 11 , 11, ifelse (
                  db$score >= 9, 10, ifelse (
                    db$score >= 8, 9, ifelse (
                      db$score >= 6, 8, ifelse (
                        db$score >= 5, 7, ifelse (
                          db$score >= 4, 6, ifelse (
                            
                            db$score >= 3, 4, ifelse (
                              
                              db$score <= 2, 2, NA ))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      
      db$score >= 18 , "95-97" , ifelse (
        
        db$score >= 16 , "82-89" , ifelse (
          db$score >= 13 , "72-81" , ifelse (
            db$score >= 11 , "60-71" , ifelse (
              db$score >= 9, "41-59" , ifelse (
                db$score >= 8, "29-40" , ifelse (
                  db$score >= 6, "19-28" , ifelse (
                    db$score >= 5, "11-18" , ifelse (
                      db$score >= 4, "6-10" , ifelse (
                        
                        db$score >= 3, "2" , ifelse (
                          
                          db$score <= 2, "<1" , NA )))))))))))))) 
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  21, 18, ifelse (
        
        db$score >= 20, 16, ifelse (
          db$score >= 18 , 15, ifelse (
            db$score >= 17, 14, ifelse (
              db$score >= 14 , 13, ifelse (
                db$score >= 12 , 12, ifelse (
                  db$score >= 10 , 11, ifelse (
                    db$score >= 8, 10, ifelse (
                      db$score >= 7, 9, ifelse (
                        db$score >= 5, 8, ifelse (
                          
                          db$score >= 4, 6, ifelse (
                            
                            db$score >= 3, 4, ifelse (
                              
                              db$score <= 2, 2, NA ))))))))))))))
    
# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  21, "> 99" , ifelse (
    
    db$score >= 20, "98" , ifelse (
      db$score >= 18 , "95-97" , ifelse (
        db$score >= 17, "90-94" , ifelse (
          db$score >= 14 , "82-89" , ifelse (
            db$score >= 12 , "72-81" , ifelse (
              db$score >= 10 , "60-71" , ifelse (
                db$score >= 8, "41-59" , ifelse (
                  db$score >= 7, "29-40" , ifelse (
                    db$score >= 5, "19-28" , ifelse (
                      
                      db$score >= 4, "6-10" , ifelse (
                        
                        db$score >= 3, "2" , ifelse (
                          
                          db$score <= 2, "<1" , NA )))))))))))))) 
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  19, 18, ifelse (
        
        db$score >= 18, 16, ifelse (
          db$score >= 15 , 15, ifelse (
            
            db$score >= 12 , 13, ifelse (
              db$score >= 11, 12, ifelse (
                db$score >= 9, 11, ifelse (
                  db$score >= 7, 10, ifelse (
                    db$score >= 6, 9, ifelse (
                      
                      db$score >= 5, 7, ifelse (
                        db$score >= 4, 6, ifelse (
                          
                          
                          db$score >= 3, 3, ifelse (
                            db$score <= 2, 2, NA )))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  19, "> 99" , ifelse (
    
    db$score >= 18, "98" , ifelse (
      db$score >= 15 , "95-97" , ifelse (
        
        db$score >= 12 , "82-89" , ifelse (
          db$score >= 11, "72-81" , ifelse (
            db$score >= 9, "60-71" , ifelse (
              db$score >= 7, "41-59" , ifelse (
                db$score >= 6, "29-40" , ifelse (
                  
                  db$score >= 5, "11-18" , ifelse (
                    db$score >= 4, "6-10" , ifelse (
                      
                      
                      db$score >= 3, "1" , ifelse (
                        db$score <= 2, "<1" , NA ))))))))))))) 
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$mscale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        
        db$score >= 15 , 16, ifelse (
          db$score >= 14, 15, ifelse (
            db$score >= 13, 14, ifelse (
              db$score >= 12, 13, ifelse (
                db$score >= 11, 12, ifelse (
                  db$score >= 9, 11, ifelse (
                    db$score >= 7, 10, ifelse (
                      db$score >= 6, 9, ifelse (
                        db$score >= 5, 8, ifelse (
                          
                          
                          db$score >= 4, 5, ifelse (
                            db$score >= 3, 4, ifelse (
                              
                              db$score <= 2, 2, NA ))))))))))))))

# percentile score

db$mpercentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    
    db$score >= 15 , "98" , ifelse (
      db$score >= 14, "95-97" , ifelse (
        db$score >= 13, "90-94" , ifelse (
          db$score >= 12, "82-89" , ifelse (
            db$score >= 11, "72-81" , ifelse (
              db$score >= 9, "60-71" , ifelse (
                db$score >= 7, "41-59" , ifelse (
                  db$score >= 6, "29-40" , ifelse (
                    db$score >= 5, "19-28" , ifelse (
                      
                      
                      db$score >= 4, "3-5" , ifelse (
                        db$score >= 3, "2" , ifelse (
                          
                          db$score <= 2, "<1" , NA ))))))))))))))  
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 2, db$mscale_score + 2, ifelse(
     db$education_years >= 3  & db$education_years <= 7, db$mscale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$mscale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 12, db$mscale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$mscale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
  db$NSSae_m <- db$mscale_score - (0.24352*(db$education_years_adj-12)) 
  
  return(db)
}


