# digit span forward 
#
# Reference:
# Peña-Casanova, J., Quiñones-Ubeda, S., Quintana-Aparicio, M., et al., 2009. Neuronorma
# study team. Spanish multicenter normative studies (neuronorma project): norms for verbal span, 
# visuospatial span, letter and number sequencing, trail making test, and symbol digit modalities test. 
# Arch. Clin. Neuropsychol. 24 (4), 321–341.
# 
# 
ds_forward <- function(score, age, education_years){
  
  ds_forward_db <- data.frame(score = score, age = age, education_years = education_years)
  ds_forward_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(ds_forward_db)) {
    res <- ds_foward_scale_score(score = ds_forward_db[i, "score"], 
                              age = ds_forward_db[i, "age"],
                              education_years = ds_forward_db[i, "education_years"])
    ds_forward_new <- rbind(ds_forward_new, res)
  }
  
  return(ds_forward_new[,c("ds_forward_scale_score", "ds_forward_percentil_range", "ds_forward_NSSae")])
}

ds_foward_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        
        
        db$score >= 8, 15, ifelse (
          db$score >= 7, 14, ifelse (
            
            
            db$score >= 6, 11, ifelse (
              
              db$score >= 5, 9, ifelse (
                
                
                
                db$score >= 4, 5, ifelse (
                  
                  
                  db$score <= 3, 2, NA )))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    
    
    db$score >= 8, "95-97" , ifelse (
      db$score >= 7, "90-94" , ifelse (
        
        
        db$score >= 6, "60-71" , ifelse (
          
          db$score >= 5, "29-40" , ifelse (
            
            
            
            db$score >= 4, "3-5" , ifelse (
              
              
              db$score <= 3, "<1" , NA )))))))))
    
    
    
    
    
  }
  
  
  ################################## TABLE 4 ##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        
        db$score >= 8, 16, ifelse (
          
          db$score >= 7, 14, ifelse (
            
            db$score >= 6, 12, ifelse (
              
              db$score >= 5, 10, ifelse (
                
                
                
                db$score >= 4, 6, ifelse (
                  
                  
                  
                  db$score <= 3, 2, NA )))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    
    db$score >= 8, "98" , ifelse (
      
      db$score >= 7, "90-94" , ifelse (
        
        db$score >= 6, "72-81" , ifelse (
          
          db$score >= 5, "41-59" , ifelse (
            
            
            
            db$score >= 4, "6-10" , ifelse (
              
              
              
              db$score <= 3, "<1" , NA )))))))))
    
    
    
    
    
  }
  
  
  ############################# TABLE 5 ###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        
        db$score >= 8, 16, ifelse (
          
          db$score >= 7, 14, ifelse (
            
            db$score >= 6, 12, ifelse (
              
              db$score >= 5, 10, ifelse (
                
                
                db$score >= 4, 7, ifelse (
                  
                  
                  
                  
                  db$score <= 3, 2, NA )))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    
    db$score >= 8, "98" , ifelse (
      
      db$score >= 7, "90-94" , ifelse (
        
        db$score >= 6, "72-81" , ifelse (
          
          db$score >= 5, "41-59" , ifelse (
            
            
            db$score >= 4, "11-18" , ifelse (
              
              
              
              
              db$score <= 3, "<1" , NA )))))))))
    
    
    
  }
  
  
  ########################### TABLE 6 #####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        
        db$score >= 8, 16, ifelse (
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              
              
              db$score >= 5, 10, ifelse (
                
                
                db$score >= 4, 7, ifelse (
                  
                  
                  
                  
                  db$score <= 3, 2, NA )))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    
    db$score >= 8, "98" , ifelse (
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          
          
          db$score >= 5, "41-59" , ifelse (
            
            
            db$score >= 4, "11-18" , ifelse (
              
              
              
              
              db$score <= 3, "<1" , NA )))))))))
    
    
  }
  
  
  ############################ TABLE 7 ####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              
              
              db$score >= 5, 10, ifelse (
                
                db$score >= 4, 8, ifelse (
                  
                  
                  
                  db$score >= 3, 4, ifelse (
                    
                    db$score <= 2, 2, NA ))))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          
          
          db$score >= 5, "41-59" , ifelse (
            
            db$score >= 4, "19-28" , ifelse (
              
              
              
              db$score >= 3, "2" , ifelse (
                
                db$score <= 2, "<1" , NA ))))))))))
    
  }
  
  
  ############################## TABLE 8 ##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              
              
              db$score >= 5, 10, ifelse (
                
                db$score >= 4, 8, ifelse (
                  
                  
                  
                  db$score >= 3, 4, ifelse (
                    
                    db$score <= 2, 2, NA ))))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          
          
          db$score >= 5, "41-59" , ifelse (
            
            db$score >= 4, "19-28" , ifelse (
              
              
              
              db$score >= 3, "2" , ifelse (
                
                db$score <= 2, "<1" , NA ))))))))))
    
    
  }
  
  
  ############################## TABLE 9 ##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          
          db$score >= 7, 15, ifelse (
            
            db$score >= 6, 13, ifelse (
              
              
              db$score >= 5, 10, ifelse (
                
                
                db$score >= 4, 7, ifelse (
                  
                  
                  db$score >= 3, 4, ifelse (
                    
                    db$score <= 2, 2, NA ))))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      
      db$score >= 7, "95-97" , ifelse (
        
        db$score >= 6, "82-89" , ifelse (
          
          
          db$score >= 5, "41-59" , ifelse (
            
            
            db$score >= 4, "11-18" , ifelse (
              
              
              db$score >= 3, "2" , ifelse (
                
                db$score <= 2, "<1" , NA ))))))))) )
    
    
  }
  
  
  
  ############################## TABLE 10 ##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 9, 18, ifelse (
        db$score >= 8, 17, ifelse (
          db$score >= 7, 16, ifelse (
            
            
            db$score >= 6, 13, ifelse (
              
              
              db$score >= 5, 10, ifelse (
                
                
                db$score >= 4, 7, ifelse (
                  
                  
                  
                  db$score >= 3, 3, ifelse (
                    db$score <= 2, 2, NA ))))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 9, "> 99" , ifelse (
    db$score >= 8, "99" , ifelse (
      db$score >= 7, "98" , ifelse (
        
        
        db$score >= 6, "82-89" , ifelse (
          
          
          db$score >= 5, "41-59" , ifelse (
            
            
            db$score >= 4, "11-18" , ifelse (
              
              
              
              db$score >= 3, "1" , ifelse (
                db$score <= 2, "<1" , NA ))))))))))
    
    
    
  }
  
  
  ############################## TABLE 11 ##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 8, 18, ifelse (
        db$score >= 7, 17, ifelse (
          
          
          
          db$score >= 6, 13, ifelse (
            
            
            db$score >= 5, 10, ifelse (
              
              
              db$score >= 4, 7, ifelse (
                
                
                
                db$score >= 3, 3, ifelse (
                  db$score <= 2, 2, NA )))))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 8, "> 99" , ifelse (
    db$score >= 7, "99" , ifelse (
      
      
      
      db$score >= 6, "82-89" , ifelse (
        
        
        db$score >= 5, "41-59" , ifelse (
          
          
          db$score >= 4, "11-18" , ifelse (
            
            
            
            db$score >= 3, "1" , ifelse (
              db$score <= 2, "<1" , NA )))))))))
    
    
  }
  
  
  ############################# TABLE 12 ###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$ds_foward_scale_score <- with ( db, ifelse (
      is.na(db$score), NA, ifelse (
      db$score >= 7, 18, ifelse (
        
        
        
        
        db$score >= 6, 13, ifelse (
          
          db$score >= 5, 11, ifelse (
            
            
            db$score >= 4, 8, ifelse (
              
              
              
              
              
              db$score <= 3, 2, NA )))))))

# percentile score

db$ds_forward_percentil_range <- with (db, ifelse (
  is.na(db$score), NA, ifelse (
  db$score >= 7, "> 99" , ifelse (
    
    
    
    
    db$score >= 6, "82-89" , ifelse (
      
      db$score >= 5, "60-71" , ifelse (
        
        
        db$score >= 4, "19-28" , ifelse (
          
          
          
          
          
          db$score <= 3, "<1" , NA )))))))
    
    
  }
  
  
  # Educational level adjust 
  db$ds_forward_education_years_adj <- with(db, ifelse(
    is.na(db$ds_foward_scale_score), NA, ifelse (
    db$education_years >= 0  & db$education_years <= 2, db$ds_foward_scale_score + 2, ifelse(
    db$education_years >= 3  & db$education_years <= 7, db$ds_foward_scale_score + 1, ifelse(
      db$education_years >= 8  & db$education_years <= 12, db$ds_foward_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 16, db$ds_foward_scale_score - 1, ifelse(
          db$education_years >= 17  & db$education_years <= 20, db$ds_foward_scale_score - 2, ifelse(
            
            ))))))))
  
  
  # NSSae
  db$ds_forward_NSSae <- with(db, ifelse(
    is.na(db$ds_forward_education_years_adj), NA, ifelse (
      !is.na(db$ds_forward_education_years_adj), db$ds_foward_scale_score - (0.21327*(db$ds_forward_education_years_adj-12))  )))
  
  return(db)
}
