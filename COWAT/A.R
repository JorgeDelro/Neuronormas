#A
# install.packages("readxl")
library(readxl)

db <- read_xls("COG_BRUTO.xls")



a <- a_function(score = db$Q_COG_COWAT_a_PRE,
                            age = db$AGE_PRE,
                            education_years = db$EDUCATIONAL_LEVEL)

# Function GORDA
a_function <- function(score, age, education_years){
  
  a <- data.frame(score = score, age = age, education_years = education_years)
  a_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(a)) {
    res <- ascale_score(score = a[i, "score"], 
                              age = a[i, "age"],
                              education_years = a[i, "education_years"])
    a_new <- rbind(a_new, res)
  }
  
  return(a_new)
}

ascale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  21, 18, ifelse (
        db$score >= 20, 17, ifelse (
          db$score >= 18 , 16, ifelse (
            db$score >= 17, 15, ifelse (
              db$score >= 15 , 14, ifelse (
                db$score >= 13 , 13, ifelse (
                  db$score >= 11 , 12, ifelse (
                    db$score >= 10, 11, ifelse (
                      db$score >= 8, 10, ifelse (
                        db$score >= 7, 9, ifelse (
                          db$score >= 5, 8, ifelse (
                            db$score >= 4, 7, ifelse (
                              db$score >= 3, 6, ifelse (
                                db$score >= 1, 5, ifelse (
                                  
                                  
                                  db$score <= 0, 2, NA ))))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  21, "> 99" , ifelse (
    db$score >= 20, "99" , ifelse (
      db$score >= 18 , "98" , ifelse (
        db$score >= 17, "95-97" , ifelse (
          db$score >= 15 , "90-94" , ifelse (
            db$score >= 13 , "82-89" , ifelse (
              db$score >= 11 , "72-81" , ifelse (
                db$score >= 10, "60-71" , ifelse (
                  db$score >= 8, "41-59" , ifelse (
                    db$score >= 7, "29-40" , ifelse (
                      db$score >= 5, "19-28" , ifelse (
                        db$score >= 4, "11-18" , ifelse (
                          db$score >= 3, "6-10" , ifelse (
                            db$score >= 1, "3-5" , ifelse (
                              
                              
                              db$score <= 0, "<1" , NA )))))))))))))))) 
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  21, 18, ifelse (
        db$score >= 20, 17, ifelse (
          
          db$score >= 17 , 15, ifelse (
            db$score >= 15 , 14, ifelse (
              db$score >= 13 , 13, ifelse (
                db$score >= 11 , 12, ifelse (
                  db$score >= 9, 11, ifelse (
                    db$score >= 7, 10, ifelse (
                      db$score >= 6, 9, ifelse (
                        db$score >= 5, 8, ifelse (
                          db$score >= 3, 7, ifelse (
                            db$score >= 1, 6, ifelse (
                              
                              
                              
                              db$score <= 0, 2, NA ))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  21, "> 99" , ifelse (
    db$score >= 20, "99" , ifelse (
      
      db$score >= 17 , "95-97" , ifelse (
        db$score >= 15 , "90-94" , ifelse (
          db$score >= 13 , "82-89" , ifelse (
            db$score >= 11 , "72-81" , ifelse (
              db$score >= 9, "60-71" , ifelse (
                db$score >= 7, "41-59" , ifelse (
                  db$score >= 6, "29-40" , ifelse (
                    db$score >= 5, "19-28" , ifelse (
                      db$score >= 3, "11-18" , ifelse (
                        db$score >= 1, "6-10" , ifelse (
                          
                          
                          
                          db$score <= 0, "<1" , NA ))))))))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  21, 18, ifelse (
        db$score >= 20, 17, ifelse (
          db$score >= 19, 16, ifelse (
            db$score >= 16 , 15, ifelse (
              db$score >= 14 , 14, ifelse (
                db$score >= 11 , 13, ifelse (
                  db$score >= 10, 12, ifelse (
                    db$score >= 9, 11, ifelse (
                      db$score >= 7, 10, ifelse (
                        db$score >= 5, 9, ifelse (
                          db$score >= 4, 8, ifelse (
                            db$score >= 3, 7, ifelse (
                              db$score >= 1, 6, ifelse (
                                
                                
                                
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  21, "> 99" , ifelse (
    db$score >= 20, "99" , ifelse (
      db$score >= 19, "98" , ifelse (
        db$score >= 16 , "95-97" , ifelse (
          db$score >= 14 , "90-94" , ifelse (
            db$score >= 11 , "82-89" , ifelse (
              db$score >= 10, "72-81" , ifelse (
                db$score >= 9, "60-71" , ifelse (
                  db$score >= 7, "41-59" , ifelse (
                    db$score >= 5, "29-40" , ifelse (
                      db$score >= 4, "19-28" , ifelse (
                        db$score >= 3, "11-18" , ifelse (
                          db$score >= 1, "6-10" , ifelse (
                            
                            
                            
                            db$score <= 0, "<1" , NA ))))))))))))))) 
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  20, 18, ifelse (
        db$score >= 18 , 17, ifelse (
          db$score >= 17, 16, ifelse (
            db$score >= 15 , 15, ifelse (
              db$score >= 14, 14, ifelse (
                db$score >= 11 , 13, ifelse (
                  db$score >= 10, 12, ifelse (
                    db$score >= 9, 11, ifelse (
                      db$score >= 6, 10, ifelse (
                        db$score >= 5, 9, ifelse (
                          db$score >= 4, 8, ifelse (
                            db$score >= 2, 7, ifelse (
                              db$score >= 1, 6, ifelse (
                                
                                
                                
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  20, "> 99" , ifelse (
    db$score >= 18 , "99" , ifelse (
      db$score >= 17, "98" , ifelse (
        db$score >= 15 , "95-97" , ifelse (
          db$score >= 14, "90-94" , ifelse (
            db$score >= 11 , "82-89" , ifelse (
              db$score >= 10, "72-81" , ifelse (
                db$score >= 9, "60-71" , ifelse (
                  db$score >= 6, "41-59" , ifelse (
                    db$score >= 5, "29-40" , ifelse (
                      db$score >= 4, "19-28" , ifelse (
                        db$score >= 2, "11-18" , ifelse (
                          db$score >= 1, "6-10" , ifelse (
                            
                            
                            
                            db$score <= 0, "<1" , NA )))))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        db$score >= 16, 17, ifelse (
          db$score >= 15, 16, ifelse (
            db$score >= 14, 15, ifelse (
              db$score >= 13, 14, ifelse (
                db$score >= 11 , 13, ifelse (
                  db$score >= 10, 12, ifelse (
                    db$score >= 8, 11, ifelse (
                      db$score >= 6, 10, ifelse (
                        db$score >= 5, 9, ifelse (
                          db$score >= 4, 8, ifelse (
                            db$score >= 3, 7, ifelse (
                              db$score >= 2, 6, ifelse (
                                db$score >= 1, 5, ifelse (
                                  
                                  
                                  db$score <= 0, 2, NA ))))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    db$score >= 16, "99" , ifelse (
      db$score >= 15, "98" , ifelse (
        db$score >= 14, "95-97" , ifelse (
          db$score >= 13, "90-94" , ifelse (
            db$score >= 11 , "82-89" , ifelse (
              db$score >= 10, "72-81" , ifelse (
                db$score >= 8, "60-71" , ifelse (
                  db$score >= 6, "41-59" , ifelse (
                    db$score >= 5, "29-40" , ifelse (
                      db$score >= 4, "19-28" , ifelse (
                        db$score >= 3, "11-18" , ifelse (
                          db$score >= 2, "6-10" , ifelse (
                            db$score >= 1, "3-5" , ifelse (
                              
                              
                              db$score <= 0, "<1" , NA )))))))))))))))) 
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        db$score >= 16, 17, ifelse (
          db$score >= 15, 16, ifelse (
            db$score >= 14, 15, ifelse (
              db$score >= 12 , 14, ifelse (
                db$score >= 11, 13, ifelse (
                  db$score >= 9, 12, ifelse (
                    db$score >= 7, 11, ifelse (
                      db$score >= 5, 10, ifelse (
                        db$score >= 4, 9, ifelse (
                          db$score >= 3, 8, ifelse (
                            db$score >= 2, 7, ifelse (
                              db$score >= 1, 6, ifelse (
                                
                                
                                
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    db$score >= 16, "99" , ifelse (
      db$score >= 15, "98" , ifelse (
        db$score >= 14, "95-97" , ifelse (
          db$score >= 12 , "90-94" , ifelse (
            db$score >= 11, "82-89" , ifelse (
              db$score >= 9, "72-81" , ifelse (
                db$score >= 7, "60-71" , ifelse (
                  db$score >= 5, "41-59" , ifelse (
                    db$score >= 4, "29-40" , ifelse (
                      db$score >= 3, "19-28" , ifelse (
                        db$score >= 2, "11-18" , ifelse (
                          db$score >= 1, "6-10" , ifelse (
                            
                            
                            
                            db$score <= 0, "<1" , NA ))))))))))))))) 
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        db$score >= 16, 17, ifelse (
          db$score >= 15, 16, ifelse (
            db$score >= 13 , 15, ifelse (
              db$score >= 11 , 14, ifelse (
                db$score >= 10, 13, ifelse (
                  db$score >= 8, 12, ifelse (
                    db$score >= 7, 11, ifelse (
                      db$score >= 5, 10, ifelse (
                        db$score >= 4, 9, ifelse (
                          db$score >= 3, 8, ifelse (
                            db$score >= 2, 7, ifelse (
                              db$score >= 1, 6, ifelse (
                                
                                
                                
                                db$score <= 0, 2, NA )))))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    db$score >= 16, "99" , ifelse (
      db$score >= 15, "98" , ifelse (
        db$score >= 13 , "95-97" , ifelse (
          db$score >= 11 , "90-94" , ifelse (
            db$score >= 10, "82-89" , ifelse (
              db$score >= 8, "72-81" , ifelse (
                db$score >= 7, "60-71" , ifelse (
                  db$score >= 5, "41-59" , ifelse (
                    db$score >= 4, "29-40" , ifelse (
                      db$score >= 3, "19-28" , ifelse (
                        db$score >= 2, "11-18" , ifelse (
                          db$score >= 1, "6-10" , ifelse (
                            
                            
                            
                            db$score <= 0, "<1" , NA ))))))))))))))) 
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        db$score >= 16, 17, ifelse (
          db$score >= 13 , 16, ifelse (
            db$score >= 11 , 15, ifelse (
              
              db$score >= 9, 13, ifelse (
                db$score >= 7, 12, ifelse (
                  db$score >= 6, 11, ifelse (
                    db$score >= 5, 10, ifelse (
                      db$score >= 3, 9, ifelse (
                        
                        db$score >= 2, 7, ifelse (
                          db$score >= 1, 6, ifelse (
                            
                            
                            
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    db$score >= 16, "99" , ifelse (
      db$score >= 13 , "98" , ifelse (
        db$score >= 11 , "95-97" , ifelse (
          
          db$score >= 9, "82-89" , ifelse (
            db$score >= 7, "72-81" , ifelse (
              db$score >= 6, "60-71" , ifelse (
                db$score >= 5, "41-59" , ifelse (
                  db$score >= 3, "29-40" , ifelse (
                    
                    db$score >= 2, "11-18" , ifelse (
                      db$score >= 1, "6-10" , ifelse (
                        
                        
                        
                        db$score <= 0, "<1" , NA ))))))))))))) 
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  14, 18, ifelse (
        
        db$score >= 13, 16, ifelse (
          db$score >= 11 , 15, ifelse (
            db$score >= 9, 14, ifelse (
              db$score >= 8, 13, ifelse (
                db$score >= 6, 12, ifelse (
                  db$score >= 5, 11, ifelse (
                    db$score >= 4, 10, ifelse (
                      
                      db$score >= 3, 8, ifelse (
                        db$score >= 2, 7, ifelse (
                          db$score >= 1, 6, ifelse (
                            
                            
                            
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  14, "> 99" , ifelse (
    
    db$score >= 13, "98" , ifelse (
      db$score >= 11 , "95-97" , ifelse (
        db$score >= 9, "90-94" , ifelse (
          db$score >= 8, "82-89" , ifelse (
            db$score >= 6, "72-81" , ifelse (
              db$score >= 5, "60-71" , ifelse (
                db$score >= 4, "41-59" , ifelse (
                  
                  db$score >= 3, "19-28" , ifelse (
                    db$score >= 2, "11-18" , ifelse (
                      db$score >= 1, "6-10" , ifelse (
                        
                        
                        
                        db$score <= 0, "<1" , NA )))))))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$ascale_score <- with ( db, ifelse (
      db$score >=  17, 18, ifelse (
        db$score >= 16, 17, ifelse (
          db$score >= 13 , 16, ifelse (
            db$score >= 11 , 15, ifelse (
              
              db$score >= 9, 13, ifelse (
                db$score >= 7, 12, ifelse (
                  db$score >= 6, 11, ifelse (
                    db$score >= 5, 10, ifelse (
                      db$score >= 3, 9, ifelse (
                        
                        db$score >= 2, 7, ifelse (
                          db$score >= 1, 6, ifelse (
                            
                            
                            
                            db$score <= 0, 2, NA )))))))))))))

# percentile score

db$apercentil_range <- with (db, ifelse (
  db$score >=  17, "> 99" , ifelse (
    db$score >= 16, "99" , ifelse (
      db$score >= 13 , "98" , ifelse (
        db$score >= 11 , "95-97" , ifelse (
          
          db$score >= 9, "82-89" , ifelse (
            db$score >= 7, "72-81" , ifelse (
              db$score >= 6, "60-71" , ifelse (
                db$score >= 5, "41-59" , ifelse (
                  db$score >= 3, "29-40" , ifelse (
                    
                    db$score >= 2, "11-18" , ifelse (
                      db$score >= 1, "6-10" , ifelse (
                        
                        
                        
                        db$score <= 0, "<1" , NA ))))))))))))) 
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years < 1, db$ascale_score + 3, ifelse(
    db$education_years >= 1  & db$education_years <= 4, db$ascale_score + 2, ifelse(
    db$education_years >= 5  & db$education_years <= 8, db$ascale_score + 1, ifelse(
      db$education_years >= 9  & db$education_years <= 12, db$ascale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 15, db$ascale_score - 1, ifelse(
          db$education_years >= 16  & db$education_years <= 19, db$ascale_score - 2, ifelse(
            db$education_years > 19  & db$education_years <= 20, db$ascale_score- 3, ifelse(
            )))))))))
  
  
  # NSSae
  db$NSSae_a <- db$ascale_score - (0.25483*(db$education_years_adj-12)) 
  
  return(db)
}
