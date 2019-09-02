# Tower of London - Total corrected score

# Reference
# Peña-Casanova, J., Quiñones-Ubeda, S., Gramunt-Fombuena,N., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for the stroop color-word 
# interference test and the tower of london-drexel. 
# Arch. Clin. Neuropsychol. 24 (4), 413-29.
# 

# Function GORDA
TOL_CS <- function(score, age, education_years){
  
  TOL_CS_db <- data.frame(score = score, age = age, education_years = education_years)
  TOL_CS_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(TOL_CS_db)) {
    res <- TOL_CS_scale_score(score = TOL_CS_db[i, "score"], 
                              age = TOL_CS_db[i, "age"],
                              education_years = TOL_CS_db[i, "education_years"])
    TOL_CS_new <- rbind(TOL_CS_new, res)
  }
  
  return(TOL_CS_new)
}

TOL_CS_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 10, 18, ifelse (
        db$score >= 9, 17, ifelse (
          db$score >= 8, 16, ifelse (
            
            db$score >= 7, 14, ifelse (
              db$score >= 6, 13, ifelse (
                
                db$score >= 5, 11, ifelse (
                  db$score >= 3, 10, ifelse (
                    
                    db$score >= 2, 8, ifelse (
                      db$score >= 1, 7, ifelse (
                        
                        
                        
                        
                        db$score <= 0, 2, NA )))))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 10, "> 99" , ifelse (
    db$score >= 9, "99" , ifelse (
      db$score >= 8, "98" , ifelse (
        
        db$score >= 7, "90-94" , ifelse (
          db$score >= 6, "82-89" , ifelse (
            
            db$score >= 5, "60-71" , ifelse (
              db$score >= 3, "41-59" , ifelse (
                
                db$score >= 2, "19-28" , ifelse (
                  db$score >= 1, "11-18" , ifelse (
                    
                    
                    
                    
                    db$score <= 0, "<1" , NA )))))))))))

    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              db$score >= 5, 12, ifelse (
                db$score >= 4, 11, ifelse (
                  db$score >= 3, 10, ifelse (
                    
                    db$score >= 2, 8, ifelse (
                      db$score >= 1, 7, ifelse (
                        
                        
                        
                        
                        db$score <= 0, 2, NA )))))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          db$score >= 5, "72-81" , ifelse (
            db$score >= 4, "60-71" , ifelse (
              db$score >= 3, "41-59" , ifelse (
                
                db$score >= 2, "19-28" , ifelse (
                  db$score >= 1, "11-18" , ifelse (
                    
                    
                    
                    
                    db$score <= 0, "<1" , NA )))))))))))
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 8, 18, ifelse (
        
        db$score >= 7, 16, ifelse (
          
          db$score >= 6, 14, ifelse (
            
            db$score >= 5, 12, ifelse (
              db$score >= 4, 11, ifelse (
                db$score >= 3, 10, ifelse (
                  
                  db$score >= 2, 8, ifelse (
                    db$score >= 1, 7, ifelse (
                      
                      
                      
                      
                      db$score <= 0, 2, NA ))))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 8, "> 99" , ifelse (
    
    db$score >= 7, "98" , ifelse (
      
      db$score >= 6, "90-94" , ifelse (
        
        db$score >= 5, "72-81" , ifelse (
          db$score >= 4, "60-71" , ifelse (
            db$score >= 3, "41-59" , ifelse (
              
              db$score >= 2, "19-28" , ifelse (
                db$score >= 1, "11-18" , ifelse (
                  
                  
                  
                  
                  db$score <= 0, "<1" , NA ))))))))))
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 8, 18, ifelse (
        
        
        db$score >= 7, 15, ifelse (
          
          db$score >= 6, 13, ifelse (
            db$score >= 5, 12, ifelse (
              db$score >= 4, 11, ifelse (
                db$score >= 3, 10, ifelse (
                  
                  db$score >= 2, 8, ifelse (
                    db$score >= 1, 7, ifelse (
                      
                      
                      
                      
                      db$score <= 0, 2, NA ))))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 8, "> 99" , ifelse (
    
    
    db$score >= 7, "95-97" , ifelse (
      
      db$score >= 6, "82-89" , ifelse (
        db$score >= 5, "72-81" , ifelse (
          db$score >= 4, "60-71" , ifelse (
            db$score >= 3, "41-59" , ifelse (
              
              db$score >= 2, "19-28" , ifelse (
                db$score >= 1, "11-18" , ifelse (
                  
                  
                  
                  
                  db$score <= 0, "<1" , NA ))))))))))
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 8, 18, ifelse (
        
        
        db$score >= 7, 15, ifelse (
          
          db$score >= 6, 13, ifelse (
            db$score >= 5, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                db$score >= 2, 8, ifelse (
                  db$score >= 1, 7, ifelse (
                    
                    
                    
                    
                    db$score <= 0, 2, NA )))))))))


# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 8, "> 99" , ifelse (
    
    
    db$score >= 7, "95-97" , ifelse (
      
      db$score >= 6, "82-89" , ifelse (
        db$score >= 5, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            db$score >= 2, "19-28" , ifelse (
              db$score >= 1, "11-18" , ifelse (
                
                
                
                
                db$score <= 0, "<1" , NA )))))))))
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 8, 18, ifelse (
        
        
        db$score >= 7, 15, ifelse (
          
          db$score >= 6, 13, ifelse (
            db$score >= 5, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                db$score >= 2, 8, ifelse (
                  db$score >= 1, 7, ifelse (
                    
                    
                    
                    
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 8, "> 99" , ifelse (
    
    
    db$score >= 7, "95-97" , ifelse (
      
      db$score >= 6, "82-89" , ifelse (
        db$score >= 5, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            db$score >= 2, "19-28" , ifelse (
              db$score >= 1, "11-18" , ifelse (
                
                
                
                
                db$score <= 0, "<1" , NA )))))))))
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              db$score >= 5, 12, ifelse (
                db$score >= 4, 11, ifelse (
                  db$score >= 3, 10, ifelse (
                    
                    db$score >= 2, 8, ifelse (
                      db$score >= 1, 7, ifelse (
                        
                        
                        
                        
                        db$score <= 0, 2, NA )))))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          db$score >= 5, "72-81" , ifelse (
            db$score >= 4, "60-71" , ifelse (
              db$score >= 3, "41-59" , ifelse (
                
                db$score >= 2, "19-28" , ifelse (
                  db$score >= 1, "11-18" , ifelse (
                    
                    
                    
                    
                    db$score <= 0, "<1" , NA )))))))))))
    
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              db$score >= 5, 12, ifelse (
                db$score >= 4, 11, ifelse (
                  db$score >= 3, 10, ifelse (
                    db$score >= 2, 9, ifelse (
                      
                      db$score >= 1, 7, ifelse (
                        
                        
                        
                        
                        db$score <= 0, 2, NA )))))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          db$score >= 5, "72-81" , ifelse (
            db$score >= 4, "60-71" , ifelse (
              db$score >= 3, "41-59" , ifelse (
                db$score >= 2, "29-40" , ifelse (
                  
                  db$score >= 1, "11-18" , ifelse (
                    
                    
                    
                    
                    db$score <= 0, "<1" , NA )))))))))))
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 8, 18, ifelse (
        
        db$score >= 7, 16, ifelse (
          
          
          db$score >= 5, 13, ifelse (
            
            db$score >= 4, 11, ifelse (
              db$score >= 3, 10, ifelse (
                db$score >= 2, 9, ifelse (
                  
                  
                  db$score >= 1, 6, ifelse (
                    
                    
                    
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 8, "> 99" , ifelse (
    
    db$score >= 7, "98" , ifelse (
      
      
      db$score >= 5, "82-89" , ifelse (
        
        db$score >= 4, "60-71" , ifelse (
          db$score >= 3, "41-59" , ifelse (
            db$score >= 2, "29-40" , ifelse (
              
              
              db$score >= 1, "6-10" , ifelse (
                
                
                
                db$score <= 0, "<1" , NA )))))))))
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    
    db$TOL_CS_scale_score <- with ( db, ifelse (
      db$score >= 8, 18, ifelse (
        
        db$score >= 7, 16, ifelse (
          
          db$score >= 6, 14, ifelse (
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                db$score >= 2, 8, ifelse (
                  
                  db$score >= 1, 6, ifelse (
                    
                    
                    
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$TOL_CS_percentil_range <- with (db, ifelse (
  db$score >= 8, "> 99" , ifelse (
    
    db$score >= 7, "98" , ifelse (
      
      db$score >= 6, "90-94" , ifelse (
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            db$score >= 2, "19-28" , ifelse (
              
              db$score >= 1, "6-10" , ifelse (
                
                
                
                db$score <= 0, "<1" , NA )))))))))
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 5, db$TOL_CS_scale_score + 1, ifelse(
      db$education_years >= 6  & db$education_years <= 12, db$TOL_CS_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 18, db$TOL_CS_scale_score - 1, ifelse(
          db$education_years >= 19  & db$education_years <= 20, db$TOL_CS_scale_score - 2, ifelse(
            
            ))))))
  
  
  # NSSae
  db$NSSae_TOL_CS <- db$TOL_CS_scale_score - (0.15447*(db$education_years_adj-12)) 
  
  return(db)
}
