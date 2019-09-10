# digit span backward
#
# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 


ds_backward <- function(score, age, education_years){
  
  ds_backward_db <- data.frame(score = score, age = age, education_years = education_years)
  ds_backward_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(ds_backward_db)) {
    res <- ds_backward_scale_score(score = ds_backward_db[i, "score"], 
                              age = ds_backward_db[i, "age"],
                              education_years = ds_backward_db[i, "education_years"])
    ds_backward_new <- rbind(ds_backward_new, res)
  }
  
  return(ds_backward_new[,c("ds_backward_scale_score", "ds_backward_percentil_range", "ds_backward_NSSae")])
}

ds_backward_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        
        
        db$score >= 6, 15, ifelse (
          
          db$score >= 5, 13, ifelse (
            
            
            db$score >= 4, 10, ifelse (
              db$score >= 3, 9, ifelse (
                
                
                
                
                
                db$score >= 2, 3, ifelse (
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    
    
    db$score >= 6, "95-97" , ifelse (
      
      db$score >= 5, "82-89" , ifelse (
        
        
        db$score >= 4, "41-59" , ifelse (
          db$score >= 3, "29-40" , ifelse (
            
            
            
            
            
            db$score >= 2, "1" , ifelse (
              db$score <= 0, "<1" , NA ))))))))
    
    
    
    
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        
        
        db$score >= 6, 15, ifelse (
          
          db$score >= 5, 13, ifelse (
            
            
            db$score >= 4, 10, ifelse (
              db$score >= 3, 9, ifelse (
                
                
                
                db$score >= 2, 5, ifelse (
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    
    
    db$score >= 6, "95-97" , ifelse (
      
      db$score >= 5, "82-89" , ifelse (
        
        
        db$score >= 4, "41-59" , ifelse (
          db$score >= 3, "29-40" , ifelse (
            
            
            
            db$score >= 2, "3-5" , ifelse (
              
              
              db$score <= 0, "<1" , NA )))))))) 
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        
        
        db$score >= 6, 15, ifelse (
          db$score >= 5, 14, ifelse (
            
            
            db$score >= 4, 11, ifelse (
              
              db$score >= 3, 9, ifelse (
                
                
                
                db$score >= 2, 5, ifelse (
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    
    
    db$score >= 6, "95-97" , ifelse (
      db$score >= 5, "90-94" , ifelse (
        
        
        db$score >= 4, "60-71" , ifelse (
          
          db$score >= 3, "29-40" , ifelse (
            
            
            
            db$score >= 2, "3-5" , ifelse (
              
              
              db$score <= 0, "<1" , NA ))))))))
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 6, 18, ifelse (
        
        
        
        db$score >= 5, 14, ifelse (
          
          
          db$score >= 4, 11, ifelse (
            db$score >= 3, 10, ifelse (
              
              
              
              
              db$score >= 2, 5, ifelse (
                
                
                db$score <= 0, 2, NA )))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 6, "> 99" , ifelse (
    
    
    
    db$score >= 5, "90-94" , ifelse (
      
      
      db$score >= 4, "60-71" , ifelse (
        db$score >= 3, "41-59" , ifelse (
          
          
          
          
          db$score >= 2, "3-5" , ifelse (
            
            
            db$score <= 0, "<1" , NA )))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 6, 18, ifelse (
        
        
        
        db$score >= 5, 14, ifelse (
          
          
          db$score >= 4, 11, ifelse (
            
            db$score >= 3, 9, ifelse (
              
              
              
              db$score >= 2, 5, ifelse (
                
                
                db$score <= 0, 2, NA )))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 6, "> 99" , ifelse (
    
    
    
    db$score >= 5, "90-94" , ifelse (
      
      
      db$score >= 4, "60-71" , ifelse (
        
        db$score >= 3, "29-40" , ifelse (
          
          
          
          db$score >= 2, "3-5" , ifelse (
            
            
            db$score <= 0, "<1" , NA )))))))
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          
          db$score >= 5, 14, ifelse (
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                
                
                
                db$score >= 2, 5, ifelse (
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      
      db$score >= 5, "90-94" , ifelse (
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            
            
            
            db$score >= 2, "3-5" , ifelse (
              
              
              db$score <= 0, "<1" , NA ))))))))
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          
          db$score >= 5, 14, ifelse (
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                
                
                
                db$score >= 2, 5, ifelse (
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      
      db$score >= 5, "90-94" , ifelse (
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            
            
            
            db$score >= 2, "3-5" , ifelse (
              
              
              db$score <= 0, "<1" , NA ))))))))

    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          
          db$score >= 5, 14, ifelse (
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                
                
                db$score >= 2, 6, ifelse (
                  
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      
      db$score >= 5, "90-94" , ifelse (
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            
            
            db$score >= 2, "6-10" , ifelse (
              
              
              
              db$score <= 0, "<1" , NA ))))))))
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 7, 18, ifelse (
        db$score >= 6, 17, ifelse (
          
          
          db$score >= 5, 14, ifelse (
            
            db$score >= 4, 12, ifelse (
              
              db$score >= 3, 10, ifelse (
                
                
                
                db$score >= 2, 6, ifelse (
                  
                  
                  
                  db$score <= 0, 2, NA ))))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 7, "> 99" , ifelse (
    db$score >= 6, "99" , ifelse (
      
      
      db$score >= 5, "90-94" , ifelse (
        
        db$score >= 4, "72-81" , ifelse (
          
          db$score >= 3, "41-59" , ifelse (
            
            
            
            db$score >= 2, "6-10" , ifelse (
              
              
              
              db$score <= 0, "<1" , NA ))))))))
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$ds_backward_scale_score <- with ( db, ifelse (
      db$score >= 6, 18, ifelse (
        
        
        
        db$score >= 5, 14, ifelse (
          
          db$score >= 4, 12, ifelse (
            
            db$score >= 3, 10, ifelse (
              
              
              db$score >= 2, 7, ifelse (
                
                
                
                
                db$score <= 0, 2, NA )))))))

# percentile score

db$ds_backard_percentil_range <- with (db, ifelse (
  db$score >= 6, "> 99" , ifelse (
    
    
    
    db$score >= 5, "90-94" , ifelse (
      
      db$score >= 4, "72-81" , ifelse (
        
        db$score >= 3, "41-59" , ifelse (
          
          
          db$score >= 2, "11-18" , ifelse (
            
            
            
            
            db$score <= 0, "<1" , NA )))))))
    
    
  }
  
  
  # Educational level adjust 
  db$ds_backward_education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 2, db$ds_backward_scale_score + 2, ifelse(
    db$education_years >= 3  & db$education_years <= 7, db$ds_backward_scale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$ds_backward_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 16, db$ds_backward_scale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$ds_backward_scale_score - 2, ifelse(
            
            )))))))
  
  
  # NSSae
  db$ds_backward_NSSae <- db$ds_backward_scale_score - (0.21298*(db$ds_backward_education_years_adj-12)) 
  
  return(db)
}
