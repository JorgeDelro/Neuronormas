# letter-number sequencing last item score

# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 

# 
LNS_LI <- function(score, age, education_years){
  
  LNS_LI_db <- data.frame(score = score, age = age, education_years = education_years)
  LNS_LI_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(LNS_LI_db)) {
    res <- LNS_LI_scale_score(score = LNS_LI_db[i, "score"], 
                              age = LNS_LI_db[i, "age"],
                              education_years = LNS_LI_db[i, "education_years"])
    LNS_LI_new <- rbind(LNS_LI_new, res)
  }
  
  return(LNS_LI_new[,c("LNS_LI_scale_score", "LNS_LI_percentil_range", "LNS_LI_NSSae")])
}

LNS_LI_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          
          db$score >= 5, 14, ifelse (
            
            
            db$score >= 4, 11, ifelse (
              
              db$score >= 3, 9, ifelse (
                
                
                db$score >= 2, 6, ifelse (
                  
                  
                  db$score >= 1, 3, ifelse (
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      
      db$score >= 5, "90-94" , ifelse (
        
        
        db$score >= 4, "60-71" , ifelse (
          
          db$score >= 3, "29-40" , ifelse (
            
            
            db$score >= 2, "6-10" , ifelse (
              
              
              db$score >= 1, "1" , ifelse (
                db$score <= 0, "<1" , NA ))))))))) 
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 6, 18, ifelse (
        
        
        
        db$score >= 5, 14, ifelse (
          
          
          db$score >= 4, 11, ifelse (
            
            db$score >= 3, 9, ifelse (
              
              db$score >= 2, 7, ifelse (
                
                
                
                db$score >= 1, 3, ifelse (
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 6, "> 99" , ifelse (
    
    
    
    db$score >= 5, "90-94" , ifelse (
      
      
      db$score >= 4, "60-71" , ifelse (
        
        db$score >= 3, "29-40" , ifelse (
          
          db$score >= 2, "11-18" , ifelse (
            
            
            
            db$score >= 1, "1" , ifelse (
              db$score <= 0, "<1" , NA ))))))))
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          
          db$score >= 5, 14, ifelse (
            
            
            db$score >= 4, 11, ifelse (
              db$score >= 3, 10, ifelse (
                
                
                db$score >= 2, 7, ifelse (
                  
                  
                  
                  db$score >= 1, 3, ifelse (
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      
      db$score >= 5, "90-94" , ifelse (
        
        
        db$score >= 4, "60-71" , ifelse (
          db$score >= 3, "41-59" , ifelse (
            
            
            db$score >= 2, "11-18" , ifelse (
              
              
              
              db$score >= 1, "1" , ifelse (
                db$score <= 0, "<1" , NA )))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          db$score >= 5, 15, ifelse (
            
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                db$score >= 2, 8, ifelse (
                  
                  
                  
                  
                  db$score >= 1, 3, ifelse (
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      db$score >= 5, "95-97" , ifelse (
        
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            db$score >= 2, "19-28" , ifelse (
              
              
              
              
              db$score >= 1, "1" , ifelse (
                db$score <= 0, "<1" , NA )))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          db$score >= 5, 15, ifelse (
            
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                db$score >= 2, 8, ifelse (
                  
                  
                  
                  
                  db$score >= 1, 3, ifelse (
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      db$score >= 5, "95-97" , ifelse (
        
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            db$score >= 2, "19-28" , ifelse (
              
              
              
              
              db$score >= 1, "1" , ifelse (
                db$score <= 0, "<1" , NA )))))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          db$score >= 5, 15, ifelse (
            
            db$score >= 4, 13, ifelse (
              
              
              db$score >= 3, 10, ifelse (
                db$score >= 2, 9, ifelse (
                  
                  
                  
                  
                  db$score >= 1, 4, ifelse (
                    
                    db$score <= 0, 2, NA )))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      db$score >= 5, "95-97" , ifelse (
        
        db$score >= 4, "82-89" , ifelse (
          
          
          db$score >= 3, "41-59" , ifelse (
            db$score >= 2, "29-40" , ifelse (
              
              
              
              
              db$score >= 1, "2" , ifelse (
                
                db$score <= 0, "<1" , NA )))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 6, 18, ifelse (
        db$score >= 5, 17, ifelse (
          
          
          
          db$score >= 4, 13, ifelse (
            
            
            db$score >= 3, 10, ifelse (
              db$score >= 2, 9, ifelse (
                
                
                
                db$score >= 1, 5, ifelse (
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 6, "> 99" , ifelse (
    db$score >= 5, "99" , ifelse (
      
      
      
      db$score >= 4, "82-89" , ifelse (
        
        
        db$score >= 3, "41-59" , ifelse (
          db$score >= 2, "29-40" , ifelse (
            
            
            
            db$score >= 1, "3-5" , ifelse (
              
              
              db$score <= 0, "<1" , NA ))))))))
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 6, 18, ifelse (
        
        db$score >= 5, 16, ifelse (
          
          
          db$score >= 4, 13, ifelse (
            
            db$score >= 3, 11, ifelse (
              
              db$score >= 2, 9, ifelse (
                
                
                db$score >= 1, 6, ifelse (
                  
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 6, "> 99" , ifelse (
    
    db$score >= 5, "98" , ifelse (
      
      
      db$score >= 4, "82-89" , ifelse (
        
        db$score >= 3, "60-71" , ifelse (
          
          db$score >= 2, "29-40" , ifelse (
            
            
            db$score >= 1, "6-10" , ifelse (
              
              
              
              db$score <= 0, "<1" , NA ))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 5, 18, ifelse (
        
        
        
        
        db$score >= 4, 13, ifelse (
          db$score >= 3, 12, ifelse (
            
            
            db$score >= 2, 9, ifelse (
              
              
              db$score >= 1, 6, ifelse (
                
                
                
                db$score <= 0, 2, NA )))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 5, "> 99" , ifelse (
    
    
    
    
    db$score >= 4, "82-89" , ifelse (
      db$score >= 3, "72-81" , ifelse (
        
        
        db$score >= 2, "29-40" , ifelse (
          
          
          db$score >= 1, "6-10" , ifelse (
            
            
            
            db$score <= 0, "<1" , NA )))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$LNS_LI_scale_score <- with ( db, ifelse (
      db$score >= 5, 18, ifelse (
        
        
        
        db$score >= 4, 14, ifelse (
          
          db$score >= 3, 12, ifelse (
            
            
            db$score >= 2, 9, ifelse (
              
              
              db$score >= 1, 6, ifelse (
                
                
                
                db$score <= 0, 2, NA )))))))

# percentile score

db$LNS_LI_percentil_range <- with (db, ifelse (
  db$score >= 5, "> 99" , ifelse (
    
    
    
    db$score >= 4, "90-94" , ifelse (
      
      db$score >= 3, "72-81" , ifelse (
        
        
        db$score >= 2, "29-40" , ifelse (
          
          
          db$score >= 1, "6-10" , ifelse (
            
            
            
            db$score <= 0, "<1" , NA ))))))) 
    
    
  }
  
  
  # Educational level adjust 
  db$LNS_LI_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 3, db$LNS_LI_scale_score + 2, ifelse(
    db$education_years >= 4  & db$education_years <= 7, db$LNS_LI_scale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$LNS_LI_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 16, db$LNS_LI_scale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$LNS_LI_scale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
  db$LNS_LINSSae <- db$LNS_LI_scale_score - (0.24927*(db$LNS_LI_education_years_adj-12)) 
  
  return(db)
}
