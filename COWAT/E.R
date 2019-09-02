# E
# install.packages("readxl")
library(readxl)

db <- read_xls("COG_BRUTO.xls")



e <- e_function(score = db$Q_COG_COWAT_e_PRE,
                            age = db$AGE_PRE,
                            education_years = db$EDUCATIONAL_LEVEL)

# Function GORDA
e_function <- function(score, age, education_years){
  
  e <- data.frame(score = score, age = age, education_years = education_years)
  e_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(e)) {
    res <- escale_score(score = e[i, "score"], 
                              age = e[i, "age"],
                              education_years = e[i, "education_years"])
    e_new <- rbind(e_new, res)
  }
  
  return(e_new)
}

escale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        db$score >= 22, 17, ifelse (
          db$score >= 21, 16, ifelse (
            db$score >= 18 , 15, ifelse (
              db$score >= 17, 14, ifelse (
                db$score >= 15 , 13, ifelse (
                  db$score >= 14, 12, ifelse (
                    db$score >= 12 , 11, ifelse (
                      db$score >= 10 , 10, ifelse (
                        db$score >= 9, 9, ifelse (
                          db$score >= 8, 8, ifelse (
                            db$score >= 5, 7, ifelse (
                              
                              db$score >= 3, 5, ifelse (
                                db$score >= 2, 4, ifelse (
                                  
                                  db$score <= 1, 2, NA ))))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    db$score >= 22, "99" , ifelse (
      db$score >= 21, "98" , ifelse (
        db$score >= 18 , "95-97" , ifelse (
          db$score >= 17, "90-94" , ifelse (
            db$score >= 15 , "82-89" , ifelse (
              db$score >= 14, "72-81" , ifelse (
                db$score >= 12 , "60-71" , ifelse (
                  db$score >= 10 , "41-59" , ifelse (
                    db$score >= 9, "29-40" , ifelse (
                      db$score >= 8, "19-28" , ifelse (
                        db$score >= 5, "11-18" , ifelse (
                          
                          db$score >= 3, "3-5" , ifelse (
                            db$score >= 2, "2" , ifelse (
                              
                              db$score <= 1, "<1" , NA ))))))))))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        
        db$score >= 22, 16, ifelse (
          db$score >= 21, 15, ifelse (
            db$score >= 18 , 14, ifelse (
              db$score >= 16 , 13, ifelse (
                db$score >= 13 , 12, ifelse (
                  db$score >= 12, 11, ifelse (
                    db$score >= 9, 10, ifelse (
                      db$score >= 8, 9, ifelse (
                        db$score >= 7, 8, ifelse (
                          db$score >= 6, 7, ifelse (
                            db$score >= 4, 6, ifelse (
                              db$score >= 2, 5, ifelse (
                                
                                
                                db$score <= 1, 2, NA )))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    
    db$score >= 22, "98" , ifelse (
      db$score >= 21, "95-97" , ifelse (
        db$score >= 18 , "90-94" , ifelse (
          db$score >= 16 , "82-89" , ifelse (
            db$score >= 13 , "72-81" , ifelse (
              db$score >= 12, "60-71" , ifelse (
                db$score >= 9, "41-59" , ifelse (
                  db$score >= 8, "29-40" , ifelse (
                    db$score >= 7, "19-28" , ifelse (
                      db$score >= 6, "11-18" , ifelse (
                        db$score >= 4, "6-10" , ifelse (
                          db$score >= 2, "3-5" , ifelse (
                            
                            
                            db$score <= 1, "<1" , NA )))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 19 , 17, ifelse (
          
          db$score >= 18, 15, ifelse (
            db$score >= 16 , 14, ifelse (
              
              db$score >= 13 , 12, ifelse (
                db$score >= 11 , 11, ifelse (
                  db$score >= 9, 10, ifelse (
                    db$score >= 7, 9, ifelse (
                      db$score >= 6, 8, ifelse (
                        db$score >= 5, 7, ifelse (
                          db$score >= 4, 6, ifelse (
                            db$score >= 2, 5, ifelse (
                              
                              
                              db$score <= 1, 2, NA ))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 19 , "99" , ifelse (
      
      db$score >= 18, "95-97" , ifelse (
        db$score >= 16 , "90-94" , ifelse (
          
          db$score >= 13 , "72-81" , ifelse (
            db$score >= 11 , "60-71" , ifelse (
              db$score >= 9, "41-59" , ifelse (
                db$score >= 7, "29-40" , ifelse (
                  db$score >= 6, "19-28" , ifelse (
                    db$score >= 5, "11-18" , ifelse (
                      db$score >= 4, "6-10" , ifelse (
                        db$score >= 2, "3-5" , ifelse (
                          
                          
                          db$score <= 1, "<1" , NA ))))))))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 19 , 17, ifelse (
          
          db$score >= 18, 15, ifelse (
            db$score >= 17, 14, ifelse (
              db$score >= 16, 13, ifelse (
                db$score >= 13 , 12, ifelse (
                  db$score >= 11 , 11, ifelse (
                    db$score >= 9, 10, ifelse (
                      db$score >= 7, 9, ifelse (
                        db$score >= 6, 8, ifelse (
                          db$score >= 4, 7, ifelse (
                            
                            db$score >= 2, 5, ifelse (
                              
                              
                              db$score <= 1, 2, NA ))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 19 , "99" , ifelse (
      
      db$score >= 18, "95-97" , ifelse (
        db$score >= 17, "90-94" , ifelse (
          db$score >= 16, "82-89" , ifelse (
            db$score >= 13 , "72-81" , ifelse (
              db$score >= 11 , "60-71" , ifelse (
                db$score >= 9, "41-59" , ifelse (
                  db$score >= 7, "29-40" , ifelse (
                    db$score >= 6, "19-28" , ifelse (
                      db$score >= 4, "11-18" , ifelse (
                        
                        db$score >= 2, "3-5" , ifelse (
                          
                          
                          db$score <= 1, "<1" , NA ))))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 21, 17, ifelse (
          db$score >= 19 , 16, ifelse (
            db$score >= 18, 15, ifelse (
              db$score >= 16 , 14, ifelse (
                db$score >= 14 , 13, ifelse (
                  db$score >= 12 , 12, ifelse (
                    db$score >= 10 , 11, ifelse (
                      db$score >= 8, 10, ifelse (
                        db$score >= 7, 9, ifelse (
                          db$score >= 5, 8, ifelse (
                            
                            db$score >= 4, 6, ifelse (
                              db$score >= 1, 5, ifelse (
                                
                                
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      db$score >= 19 , "98" , ifelse (
        db$score >= 18, "95-97" , ifelse (
          db$score >= 16 , "90-94" , ifelse (
            db$score >= 14 , "82-89" , ifelse (
              db$score >= 12 , "72-81" , ifelse (
                db$score >= 10 , "60-71" , ifelse (
                  db$score >= 8, "41-59" , ifelse (
                    db$score >= 7, "29-40" , ifelse (
                      db$score >= 5, "19-28" , ifelse (
                        
                        db$score >= 4, "6-10" , ifelse (
                          db$score >= 1, "3-5" , ifelse (
                            
                            
                            db$score <= 0, "<1" , NA ))))))))))))))) 
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  20, 18, ifelse (
        
        db$score >= 19, 16, ifelse (
          db$score >= 17 , 15, ifelse (
            db$score >= 16, 14, ifelse (
              db$score >= 14 , 13, ifelse (
                db$score >= 11 , 12, ifelse (
                  db$score >= 10, 11, ifelse (
                    db$score >= 8, 10, ifelse (
                      db$score >= 7, 9, ifelse (
                        db$score >= 5, 8, ifelse (
                          db$score >= 4, 7, ifelse (
                            db$score >= 2, 6, ifelse (
                              
                              
                              db$score >= 1, 3, ifelse (
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  20, "> 99" , ifelse (
    
    db$score >= 19, "98" , ifelse (
      db$score >= 17 , "95-97" , ifelse (
        db$score >= 16, "90-94" , ifelse (
          db$score >= 14 , "82-89" , ifelse (
            db$score >= 11 , "72-81" , ifelse (
              db$score >= 10, "60-71" , ifelse (
                db$score >= 8, "41-59" , ifelse (
                  db$score >= 7, "29-40" , ifelse (
                    db$score >= 5, "19-28" , ifelse (
                      db$score >= 4, "11-18" , ifelse (
                        db$score >= 2, "6-10" , ifelse (
                          
                          
                          db$score >= 1, "1" , ifelse (
                            db$score <= 0, "<1" , NA ))))))))))))))) 
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  20, 18, ifelse (
        
        db$score >= 19, 16, ifelse (
          db$score >= 16 , 15, ifelse (
            db$score >= 15, 14, ifelse (
              db$score >= 13 , 13, ifelse (
                db$score >= 11 , 12, ifelse (
                  db$score >= 9, 11, ifelse (
                    db$score >= 7, 10, ifelse (
                      
                      db$score >= 5, 8, ifelse (
                        db$score >= 4, 7, ifelse (
                          db$score >= 2, 6, ifelse (
                            
                            
                            db$score >= 1, 3, ifelse (
                              db$score <= 0, 2, NA ))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  20, "> 99" , ifelse (
    
    db$score >= 19, "98" , ifelse (
      db$score >= 16 , "95-97" , ifelse (
        db$score >= 15, "90-94" , ifelse (
          db$score >= 13 , "82-89" , ifelse (
            db$score >= 11 , "72-81" , ifelse (
              db$score >= 9, "60-71" , ifelse (
                db$score >= 7, "41-59" , ifelse (
                  
                  db$score >= 5, "19-28" , ifelse (
                    db$score >= 4, "11-18" , ifelse (
                      db$score >= 2, "6-10" , ifelse (
                        
                        
                        db$score >= 1, "1" , ifelse (
                          db$score <= 0, "<1" , NA ))))))))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  20, 18, ifelse (
        db$score >= 19, 17, ifelse (
          db$score >= 18, 16, ifelse (
            db$score >= 16 , 15, ifelse (
              db$score >= 14 , 14, ifelse (
                db$score >= 12 , 13, ifelse (
                  db$score >= 9, 12, ifelse (
                    db$score >= 8, 11, ifelse (
                      db$score >= 7, 10, ifelse (
                        db$score >= 5, 9, ifelse (
                          db$score >= 4, 8, ifelse (
                            db$score >= 3, 7, ifelse (
                              db$score >= 2, 6, ifelse (
                                
                                db$score >= 1, 4, ifelse (
                                  
                                  db$score <= 0, 2, NA ))))))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  20, "> 99" , ifelse (
    db$score >= 19, "99" , ifelse (
      db$score >= 18, "98" , ifelse (
        db$score >= 16 , "95-97" , ifelse (
          db$score >= 14 , "90-94" , ifelse (
            db$score >= 12 , "82-89" , ifelse (
              db$score >= 9, "72-81" , ifelse (
                db$score >= 8, "60-71" , ifelse (
                  db$score >= 7, "41-59" , ifelse (
                    db$score >= 5, "29-40" , ifelse (
                      db$score >= 4, "19-28" , ifelse (
                        db$score >= 3, "11-18" , ifelse (
                          db$score >= 2, "6-10" , ifelse (
                            
                            db$score >= 1, "2" , ifelse (
                              
                              db$score <= 0, "<1" , NA ))))))))))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  18, 18, ifelse (
        
        db$score >= 17, 16, ifelse (
          db$score >= 14 , 15, ifelse (
            db$score >= 12 , 14, ifelse (
              db$score >= 10 , 13, ifelse (
                db$score >= 9, 12, ifelse (
                  db$score >= 8, 11, ifelse (
                    db$score >= 7, 10, ifelse (
                      db$score >= 5, 9, ifelse (
                        
                        db$score >= 3, 7, ifelse (
                          db$score >= 1, 6, ifelse (
                            
                            
                            
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  18, "> 99" , ifelse (
    
    db$score >= 17, "98" , ifelse (
      db$score >= 14 , "95-97" , ifelse (
        db$score >= 12 , "90-94" , ifelse (
          db$score >= 10 , "82-89" , ifelse (
            db$score >= 9, "72-81" , ifelse (
              db$score >= 8, "60-71" , ifelse (
                db$score >= 7, "41-59" , ifelse (
                  db$score >= 5, "29-40" , ifelse (
                    
                    db$score >= 3, "11-18" , ifelse (
                      db$score >= 1, "6-10" , ifelse (
                        
                        
                        
                        db$score <= 0, "<1" , NA )))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$escale_score <- with ( db, ifelse (
      db$score >=  13, 18, ifelse (
        
        
        db$score >= 11 , 15, ifelse (
          db$score >= 10, 14, ifelse (
            db$score >= 9, 13, ifelse (
              db$score >= 8, 12, ifelse (
                db$score >= 7, 11, ifelse (
                  db$score >= 6, 10, ifelse (
                    db$score >= 5, 9, ifelse (
                      db$score >= 4, 8, ifelse (
                        db$score >= 3, 7, ifelse (
                          db$score >= 2, 6, ifelse (
                            
                            
                            
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$epercentil_range <- with (db, ifelse (
  db$score >=  13, "> 99" , ifelse (
    
    
    db$score >= 11 , "95-97" , ifelse (
      db$score >= 10, "90-94" , ifelse (
        db$score >= 9, "82-89" , ifelse (
          db$score >= 8, "72-81" , ifelse (
            db$score >= 7, "60-71" , ifelse (
              db$score >= 6, "41-59" , ifelse (
                db$score >= 5, "29-40" , ifelse (
                  db$score >= 4, "19-28" , ifelse (
                    db$score >= 3, "11-18" , ifelse (
                      db$score >= 2, "6-10" , ifelse (
                        
                        
                        
                        db$score <= 0, "<1" , NA ))))))))))))) 
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$escale_score + 3, ifelse(
    db$education_years >= 1  & db$education_years <= 4, db$escale_score + 2, ifelse(
    db$education_years >= 5  & db$education_years <= 8, db$escale_score + 1, ifelse(
      db$education_years >= 9  & db$education_years <= 12, db$escale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 15, db$escale_score - 1, ifelse(
          db$education_years >= 16  & db$education_years <= 19, db$escale_score - 2, ifelse(
            db$education_years > 19  & db$education_years <= 20, db$escale_score - 3, ifelse(
            )))))))))
  
  
  # NSSae
  db$NSSae_e <- db$escale_score - (0.25448*(db$education_years_adj-12)) 
  
  return(db)
}
