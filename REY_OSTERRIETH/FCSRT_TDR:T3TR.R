# Free and Cued Selective Reminding Test (FCSRT) Total Delayed Recall / Trial 3 Total Recall (TDR/T3TR)

# install.packages("readxl")
library(readxl)

db <- read_xls("COG_BRUTO.xls")



FCSRT_TDRT3TR <- FCSRT_TDRT3TR_function(score = db$Q_COG_COWAT_FCSRT_TDRT3TR_PRE,
                            age = db$AGE_PRE,
                            education_years = db$EDUCATIONAL_LEVEL)

# Function GORDA
FCSRT_TDRT3TR_function <- function(score, age, education_years){
  
  FCSRT_TDRT3TR <- data.frame(score = score, age = age, education_years = education_years)
  FCSRT_TDRT3TR_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(FCSRT_TDRT3TR)) {
    res <- FCSRT_TDRT3TR_scale_score(score = FCSRT_TDRT3TR[i, "score"], 
                              age = FCSRT_TDRT3TR[i, "age"],
                              education_years = FCSRT_TDRT3TR[i, "education_years"])
    FCSRT_TDRT3TR_new <- rbind(FCSRT_TDRT3TR_new, res)
  }
  
  return(FCSRT_TDRT3TR_new)
}

FCSRT_TDRT3TR_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  
  
  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    ## 50-56
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.15, 18, ifelse (
        db$score >= 1.09 , 17, ifelse (
          
          db$score >= 1.01 , 15, ifelse (
            db$score >= 0.95 , 14, ifelse (
              
              
              
              
              db$score >= 0.94, 9, ifelse (
                db$score >= 0.93, 8, ifelse (
                  db$score >= 0.87 , 7, ifelse (
                    
                    db$score >= 0.74 , 5, ifelse (
                      db$score >= 0.68 , 4, ifelse (
                        db$score >= 0.63 , 3, ifelse (
                          db$score <=  0.62, 2, NA ))))))))))))

    
    
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.15, "> 99" , ifelse (
          db$score >= 1.09 , "99" , ifelse (
            
            db$score >= 1.01 , "95-97" , ifelse (
              db$score >= 0.95 , "90-94" , ifelse (
                
                
                
                
                db$score >= 0.94, "29-40" , ifelse (
                  db$score >= 0.93, "19-28" , ifelse (
                    db$score >= 0.87 , "11-18" , ifelse (
                      
                      db$score >= 0.74 , "3-5" , ifelse (
                        db$score >= 0.68 , "2" , ifelse (
                          db$score >= 0.63 , "1" , ifelse (
                            db$score <=  0.62, "<1" , NA ))))))))))))
    
  }
  
  
  ##################################TABLE 4##############################
  if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.09, 18, ifelse (
        
        db$score >= 1.08, 16, ifelse (
          db$score >= 1.01 , 15, ifelse (
            db$score >= 0.95 , 14, ifelse (
              
              
              
              db$score >= 0.94, 10, ifelse (
                
                db$score >= 0.92 , 8, ifelse (
                  db$score >= 0.87 , 7, ifelse (
                    db$score >= 0.8 , 6, ifelse (
                      db$score >= 0.74 , 5, ifelse (
                        db$score >= 0.68 , 4, ifelse (
                          db$score >= 0.63 , 3, ifelse (
                            db$score <= 0.62, 2, NA ))))))))))))) 
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.09, "> 99" , ifelse (
          
          db$score >= 1.08, "98" , ifelse (
            db$score >= 1.01 , "95-97" , ifelse (
              db$score >= 0.95 , "90-94" , ifelse (
                
                
                
                db$score >= 0.94, "41-59" , ifelse (
                  
                  db$score >= 0.92 , "19-28" , ifelse (
                    db$score >= 0.87 , "11-18" , ifelse (
                      db$score >= 0.8 , "6-10" , ifelse (
                        db$score >= 0.74 , "3-5" , ifelse (
                          db$score >= 0.68 , "2" , ifelse (
                            db$score >= 0.63 , "1" , ifelse (
                              db$score <= 0.62, "<1" , NA )))))))))))))  
    
    
    
    
    
  }
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >= 1.09 , 18, ifelse (
        db$score >= 1.08, 17, ifelse (
          
          db$score >= 1.01 , 15, ifelse (
            db$score >= 0.95 , 14, ifelse (
              
              
              
              db$score >= 0.94, 10, ifelse (
                db$score >= 0.93, 9, ifelse (
                  db$score >= 0.88 , 8, ifelse (
                    db$score >= 0.8 , 7, ifelse (
                      db$score >= 0.78 , 6, ifelse (
                        db$score >= 0.68 , 5, ifelse (
                          db$score >= 0.65 , 4, ifelse (
                            db$score >= 0.63 , 3, ifelse (
                              db$score <= 0.62, 2, NA ))))))))))))))
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >= 1.09 , "> 99" , ifelse (
          db$score >= 1.08, "99" , ifelse (
            
            db$score >= 1.01 , "95-97" , ifelse (
              db$score >= 0.95 , "90-94" , ifelse (
                
                
                
                db$score >= 0.94, "41-59" , ifelse (
                  db$score >= 0.93, "29-40" , ifelse (
                    db$score >= 0.88 , "19-28" , ifelse (
                      db$score >= 0.8 , "11-18" , ifelse (
                        db$score >= 0.78 , "6-10" , ifelse (
                          db$score >= 0.68 , "3-5" , ifelse (
                            db$score >= 0.65 , "2" , ifelse (
                              db$score >= 0.63 , "1" , ifelse (
                                db$score <= 0.62, "<1" , NA )))))))))))))) 
    
    
    
  }
  
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.08, 18, ifelse (
        
        
        db$score >= 1.01 , 15, ifelse (
          db$score >= 0.95 , 14, ifelse (
            
            
            
            db$score >= 0.94, 10, ifelse (
              db$score >= 0.93, 9, ifelse (
                db$score >= 0.89 , 8, ifelse (
                  db$score >= 0.81 , 7, ifelse (
                    db$score >= 0.78 , 6, ifelse (
                      db$score >= 0.68 , 5, ifelse (
                        db$score >= 0.65 , 4, ifelse (
                          db$score >= 0.63 , 3, ifelse (
                            db$score <=  0.62, 2, NA )))))))))))))  
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.08, "> 99" , ifelse (
          
          
          db$score >= 1.01 , "95-97" , ifelse (
            db$score >= 0.95 , "90-94" , ifelse (
              
              
              
              db$score >= 0.94, "41-59" , ifelse (
                db$score >= 0.93, "29-40" , ifelse (
                  db$score >= 0.89 , "19-28" , ifelse (
                    db$score >= 0.81 , "11-18" , ifelse (
                      db$score >= 0.78 , "6-10" , ifelse (
                        db$score >= 0.68 , "3-5" , ifelse (
                          db$score >= 0.65 , "2" , ifelse (
                            db$score >= 0.63 , "1" , ifelse (
                              db$score <=  0.62, "<1" , NA )))))))))))))
    
    
  }
  
  
  ############################TABLE 7####################################
  if(db$age >= 66  & db$age < 69) {
    ## 66-68
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.18, 18, ifelse (
        db$score >= 1.11 , 17, ifelse (
          db$score >= 1.09 , 16, ifelse (
            db$score >= 1.08, 15, ifelse (
              db$score >= 1.01 , 14, ifelse (
                db$score >= 0.95 , 13, ifelse (
                  
                  
                  db$score >= 0.94, 10, ifelse (
                    db$score >= 0.93, 9, ifelse (
                      db$score >= 0.88 , 8, ifelse (
                        db$score >= 0.8 , 7, ifelse (
                          db$score >= 0.78 , 6, ifelse (
                            db$score >= 0.65 , 5, ifelse (
                              db$score >= 0.64, 4, ifelse (
                                
                                db$score <= 0.63, 2, NA )))))))))))))))
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.18, "> 99" , ifelse (
          db$score >= 1.11 , "99" , ifelse (
            db$score >= 1.09 , "98" , ifelse (
              db$score >= 1.08, "95-97" , ifelse (
                db$score >= 1.01 , "90-94" , ifelse (
                  db$score >= 0.95 , "82-89" , ifelse (
                    
                    
                    db$score >= 0.94, "41-59" , ifelse (
                      db$score >= 0.93, "29-40" , ifelse (
                        db$score >= 0.88 , "19-28" , ifelse (
                          db$score >= 0.8 , "11-18" , ifelse (
                            db$score >= 0.78 , "6-10" , ifelse (
                              db$score >= 0.65 , "3-5" , ifelse (
                                db$score >= 0.64, "2" , ifelse (
                                  
                                  db$score <= 0.63, "<1" , NA ))))))))))))))) 
    
  }
  
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.19, 18, ifelse (
        db$score >= 1.18, 17, ifelse (
          db$score >= 1.11 , 16, ifelse (
            db$score >= 1.08 , 15, ifelse (
              db$score >= 1.01 , 14, ifelse (
                db$score >= 0.95 , 13, ifelse (
                  
                  
                  db$score >= 0.93 , 10, ifelse (
                    db$score >= 0.92, 9, ifelse (
                      db$score >= 0.86 , 8, ifelse (
                        db$score >= 0.8 , 7, ifelse (
                          db$score >= 0.71 , 6, ifelse (
                            db$score >= 0.58 , 5, ifelse (
                              
                              
                              db$score <=  0.57, 2, NA ))))))))))))))  
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.19, "> 99" , ifelse (
          db$score >= 1.18, "99" , ifelse (
            db$score >= 1.11 , "98" , ifelse (
              db$score >= 1.08 , "95-97" , ifelse (
                db$score >= 1.01 , "90-94" , ifelse (
                  db$score >= 0.95 , "82-89" , ifelse (
                    
                    
                    db$score >= 0.93 , "41-59" , ifelse (
                      db$score >= 0.92, "29-40" , ifelse (
                        db$score >= 0.86 , "19-28" , ifelse (
                          db$score >= 0.8 , "11-18" , ifelse (
                            db$score >= 0.71 , "6-10" , ifelse (
                              db$score >= 0.58 , "3-5" , ifelse (
                                
                                
                                db$score <=  0.57, "<1" , NA )))))))))))))) 
    
    
  }
  
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.24, 18, ifelse (
        db$score >= 1.18 , 17, ifelse (
          db$score >= 1.16 , 16, ifelse (
            db$score >= 1.09 , 15, ifelse (
              db$score >= 1.01 , 14, ifelse (
                db$score >= 0.95 , 13, ifelse (
                  
                  
                  db$score >= 0.93 , 10, ifelse (
                    db$score >= 0.88 , 9, ifelse (
                      db$score >= 0.81 , 8, ifelse (
                        db$score >= 0.74 , 7, ifelse (
                          db$score >= 0.72 , 6, ifelse (
                            db$score >= 0.61 , 5, ifelse (
                              db$score >= 0.58 , 4, ifelse (
                                db$score >= 0.55 , 3, ifelse (
                                  db$score <=  0.54, 2, NA )))))))))))))))) 
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.24, "> 99" , ifelse (
          db$score >= 1.18 , "99" , ifelse (
            db$score >= 1.16 , "98" , ifelse (
              db$score >= 1.09 , "95-97" , ifelse (
                db$score >= 1.01 , "90-94" , ifelse (
                  db$score >= 0.95 , "82-89" , ifelse (
                    
                    
                    db$score >= 0.93 , "41-59" , ifelse (
                      db$score >= 0.88 , "29-40" , ifelse (
                        db$score >= 0.81 , "19-28" , ifelse (
                          db$score >= 0.74 , "11-18" , ifelse (
                            db$score >= 0.72 , "6-10" , ifelse (
                              db$score >= 0.61 , "3-5" , ifelse (
                                db$score >= 0.58 , "2" , ifelse (
                                  db$score >= 0.55 , "1" , ifelse (
                                    db$score <=  0.54, "<1" , NA )))))))))))))))) 
    
    
  }
  
  
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.24, 18, ifelse (
        db$score >= 1.23, 17, ifelse (
          db$score >= 1.18 , 16, ifelse (
            db$score >= 1.11 , 15, ifelse (
              db$score >= 1.08 , 14, ifelse (
                db$score >= 0.95 , 13, ifelse (
                  
                  
                  db$score >= 0.93 , 10, ifelse (
                    db$score >= 0.87 , 9, ifelse (
                      db$score >= 0.8 , 8, ifelse (
                        db$score >= 0.74 , 7, ifelse (
                          db$score >= 0.61 , 6, ifelse (
                            db$score >= 0.56 , 5, ifelse (
                              db$score >= 0.55 , 4, ifelse (
                                db$score >= 0.54, 3, ifelse (
                                  db$score <=  0.53, 2, NA ))))))))))))))))
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.24, "> 99" , ifelse (
          db$score >= 1.23, "99" , ifelse (
            db$score >= 1.18 , "98" , ifelse (
              db$score >= 1.11 , "95-97" , ifelse (
                db$score >= 1.08 , "90-94" , ifelse (
                  db$score >= 0.95 , "82-89" , ifelse (
                    
                    
                    db$score >= 0.93 , "41-59" , ifelse (
                      db$score >= 0.87 , "29-40" , ifelse (
                        db$score >= 0.8 , "19-28" , ifelse (
                          db$score >= 0.74 , "11-18" , ifelse (
                            db$score >= 0.61 , "6-10" , ifelse (
                              db$score >= 0.56 , "3-5" , ifelse (
                                db$score >= 0.55 , "2" , ifelse (
                                  db$score >= 0.54, "1" , ifelse (
                                    db$score <=  0.53, "<1" , NA )))))))))))))))) 
    
    
    
  }
  
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.23, 18, ifelse (
        
        db$score >= 1.18 , 16, ifelse (
          db$score >= 1.11 , 15, ifelse (
            db$score >= 1.08 , 14, ifelse (
              db$score >= 0.95 , 13, ifelse (
                
                
                db$score >= 0.91 , 10, ifelse (
                  db$score >= 0.83 , 9, ifelse (
                    db$score >= 0.74 , 8, ifelse (
                      db$score >= 0.72 , 7, ifelse (
                        db$score >= 0.61 , 6, ifelse (
                          db$score >= 0.55 , 5, ifelse (
                            
                            db$score >= 0.54, 3, ifelse (
                              db$score <=  0.53, 2, NA )))))))))))))) 
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.23, "> 99" , ifelse (
          
          db$score >= 1.18 , "98" , ifelse (
            db$score >= 1.11 , "95-97" , ifelse (
              db$score >= 1.08 , "90-94" , ifelse (
                db$score >= 0.95 , "82-89" , ifelse (
                  
                  
                  db$score >= 0.91 , "41-59" , ifelse (
                    db$score >= 0.83 , "29-40" , ifelse (
                      db$score >= 0.74 , "19-28" , ifelse (
                        db$score >= 0.72 , "11-18" , ifelse (
                          db$score >= 0.61 , "6-10" , ifelse (
                            db$score >= 0.55 , "3-5" , ifelse (
                              
                              db$score >= 0.54, "1" , ifelse (
                                db$score <=  0.53, "<1" , NA ))))))))))))))  
    
    
  }
  
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$FCSRT_TDRT3TR_scale_score <- with ( db, ifelse (
      db$score >=  1.18, 18, ifelse (
        
        
        db$score >= 1.11 , 15, ifelse (
          db$score >= 1.09 , 14, ifelse (
            db$score >= 0.95 , 13, ifelse (
              
              
              db$score >= 0.93 , 10, ifelse (
                db$score >= 0.83 , 9, ifelse (
                  db$score >= 0.76 , 8, ifelse (
                    db$score >= 0.63 , 7, ifelse (
                      db$score >= 0.59 , 6, ifelse (
                        db$score >= 0.55 , 5, ifelse (
                          db$score >= 0.54, 4, ifelse (
                            
                            db$score <=  0.53, 2, NA ))))))))))))) 
      
      
      db$FCSRT_TDRT3TR_percentil_range <- with (db, ifelse (  
        db$score >=  1.18, "> 99" , ifelse (
          
          
          db$score >= 1.11 , "95-97" , ifelse (
            db$score >= 1.09 , "90-94" , ifelse (
              db$score >= 0.95 , "82-89" , ifelse (
                
                
                db$score >= 0.93 , "41-59" , ifelse (
                  db$score >= 0.83 , "29-40" , ifelse (
                    db$score >= 0.76 , "19-28" , ifelse (
                      db$score >= 0.63 , "11-18" , ifelse (
                        db$score >= 0.59 , "6-10" , ifelse (
                          db$score >= 0.55 , "3-5" , ifelse (
                            db$score >= 0.54, "2" , ifelse (
                              
                              db$score <=  0.53, "<1" , NA )))))))))))))  
    
    
  }
  
  
  # Educational level adjust 
  db$education_years_adj <- with(db, ifelse(
    db$education_years >= 0  & db$education_years <= 4, db$FCSRT_TDRT3TR_scale_score + 1, ifelse(
      db$education_years >= 5  & db$education_years <= 12, db$FCSRT_TDRT3TR_scale_score, ifelse(
        db$education_years >= 13  & db$education_years <= 19, db$FCSRT_TDRT3TR_scale_score - 1, ifelse(
          db$education_years > 19  & db$education_years <= 20, db$FCSRT_TDRT3TR_scale_score - 2, ifelse(
            
            ))))))
  
  
  # NSSae
  db$NSSae_FCSRT_TDRT3TR <- db$FCSRT_TDRT3TR_scale_score - (0.12962*(db$education_years_adj-12)) 
  
  return(db)
}
