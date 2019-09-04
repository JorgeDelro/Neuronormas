# Semantic Fluency kitchen

# Casals-Coll M., Sánchez-Benavides G., Quintana M., Manerob R.M., Rognonia T., 
# Calvo L., Palomo R., Aranciva F., Tamayo F. & Peña-Casanova J. (2013) Estudios normativos españoles en población adulta joven
# (proyecto NEURONORMA jóvenes): normas para los test de fluencia verbal. Neurología, 28 (1):33-40.

# 
kitchen <- function(score, age, education_years){
  
  kitchen_db <- data.frame(score = score, age = age, education_years = education_years)
  kitchen_new <- data.frame()
  
  # NSSa
  for (i in 1:nrow(kitchen_db)) {
    res <- kitchen_scale_score(score = kitchen_db[i, "score"], 
                       age = kitchen_db[i, "age"],
                       education_years = kitchen_db[i, "education_years"])
    kitchen_new <- rbind(kitchen_new, res)
  }
  
  return(kitchen_new)
}

kitchen_scale_score <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)

  ##################################  TABLE 3  ##############################
  if(db$age >= 50  & db$age < 57) {
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 22 , 16, ifelse (
          db$score >= 20 , 15, ifelse (
            db$score >= 19, 14, ifelse (
              db$score >= 17 , 13, ifelse (
                db$score >= 15 , 11, ifelse (
                  db$score >= 14, 10, ifelse (
                    db$score >= 12 , 9, ifelse (
                      db$score >= 11, 8, ifelse (
                        db$score >= 9, 7, ifelse (
                          db$score >= 8, 5, ifelse (
                            db$score <= 7, 2, NA )))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  25, "> 99" , ifelse (
    db$score >= 22 , "98" , ifelse (
      db$score >= 20 , "95-97" , ifelse (
        db$score >= 19, "90-94" , ifelse (
          db$score >= 17 , "82-89" , ifelse (
            db$score >= 15 , "60-71" , ifelse (
              db$score >= 14, "41-59" , ifelse (
                db$score >= 12 , "29-40" , ifelse (
                  db$score >= 11, "19-28" , ifelse (
                    db$score >= 9, "11-18" , ifelse (
                      db$score >= 8, "3-5" , ifelse (
                        db$score <= 7, "<1" , NA )))))))))))))
}

  ##################################TABLE 4##############################
if(db$age >= 57  & db$age < 60) {
    ## 57-59
    # Scale_Score
    db$kitchen_scale_score <- with(db, ifelse (
      db$score >=  25, 18, ifelse (
        db$score >= 23 , 17, ifelse (
          db$score >= 22, 16, ifelse (
            db$score >= 20 , 15, ifelse (
              db$score >= 19, 14, ifelse (
                db$score >= 17 , 13, ifelse (
                  db$score >= 16, 12, ifelse (
                    db$score >= 15, 11, ifelse (
                      db$score >= 13 , 10, ifelse (
                        db$score >= 12, 9, ifelse (
                          db$score >= 11, 8, ifelse (
                            db$score >= 10, 7, ifelse (
                              db$score >= 9, 6, ifelse (
                                db$score >= 7, 5, ifelse (
                                  db$score <= 6, 2, NA )))))))))))))))) 
      
      
      # Percentil score
      db$percentil_range <- with (db, ifelse (
        db$score >=  25, "> 99" , ifelse (
          db$score >= 23 , "99" , ifelse (
            db$score >= 22, "98" , ifelse (
              db$score >= 20 , "95-97" , ifelse (
                db$score >= 19, "90-94" , ifelse (
                  db$score >= 17 , "82-89" , ifelse (
                    db$score >= 16, "72-81" , ifelse (
                      db$score >= 15, "60-71" , ifelse (
                        db$score >= 13 , "41-59" , ifelse (
                          db$score >= 12, "29-40" , ifelse (
                            db$score >= 11, "19-28" , ifelse (
                              db$score >= 10, "11-18" , ifelse (
                                db$score >= 9, "6-10" , ifelse (
                                  db$score >= 7, "3-5" , ifelse (
                                    db$score <= 6, "<1" , NA ))))))))))))))))   
}
  
  
  
  #############################TABLE 5###################################
  if(db$age >= 60  & db$age < 63) {
    ## 60-62
    # Scale_Score
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  23, 18, ifelse (
        db$score >= 21 , 17, ifelse (
          db$score >= 20, 16, ifelse (
            db$score >= 19, 15, ifelse (
              db$score >= 18, 14, ifelse (
                db$score >= 17, 13, ifelse (
                  db$score >= 16, 12, ifelse (
                    db$score >= 15, 11, ifelse (
                      db$score >= 13 , 10, ifelse (
                        db$score >= 12, 9, ifelse (
                          db$score >= 11, 8, ifelse (
                            db$score >= 10, 7, ifelse (
                              db$score >= 9, 6, ifelse (
                                db$score >= 8, 5, ifelse (
                                  db$score >= 7, 3, ifelse (
                                    db$score <= 6, 2, NA )))))))))))))))))

# percentile score
db$kitchen_percentil_range <- with(db, ifelse (
  db$score >=  23, "> 99" , ifelse (
    db$score >= 21 , "99" , ifelse (
      db$score >= 20, "98" , ifelse (
        db$score >= 19, "95-97" , ifelse (
          db$score >= 18, "90-94" , ifelse (
            db$score >= 17, "82-89" , ifelse (
              db$score >= 16, "72-81" , ifelse (
                db$score >= 15, "60-71" , ifelse (
                  db$score >= 13 , "41-59" , ifelse (
                    db$score >= 12, "29-40" , ifelse (
                      db$score >= 11, "19-28" , ifelse (
                        db$score >= 10, "11-18" , ifelse (
                          db$score >= 9, "6-10" , ifelse (
                            db$score >= 8, "3-5" , ifelse (
                              db$score >= 7, "1" , ifelse (
                                db$score <= 6, "<1" , NA ))))))))))))))))) 
  
  }
  
  ###########################TABLE 6#####################################
  if(db$age >= 63  & db$age < 66) {
    ## 63-65
    # Scale_Score
    
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 21, 17, ifelse (
          db$score >= 20, 16, ifelse (
            db$score >= 19, 15, ifelse (
              db$score >= 18, 14, ifelse (
                db$score >= 16 , 13, ifelse (
                  db$score >= 15, 11, ifelse (
                    db$score >= 13 , 10, ifelse (
                      db$score >= 12, 9, ifelse (
                        db$score >= 11, 8, ifelse (
                          db$score >= 9, 7, ifelse (
                            db$score >= 8, 5, ifelse (
                              db$score >= 7, 4, ifelse (
                                db$score <= 6, 2, NA )))))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      db$score >= 20, "98" , ifelse (
        db$score >= 19, "95-97" , ifelse (
          db$score >= 18, "90-94" , ifelse (
            db$score >= 16 , "82-89" , ifelse (
              db$score >= 15, "60-71" , ifelse (
                db$score >= 13 , "41-59" , ifelse (
                  db$score >= 12, "29-40" , ifelse (
                    db$score >= 11, "19-28" , ifelse (
                      db$score >= 9, "11-18" , ifelse (
                        db$score >= 8, "3-5" , ifelse (
                          db$score >= 7, "2" , ifelse (
                            db$score <= 6, "<1" , NA )))))))))))))))
}
############################TABLE 7####################################
if(db$age >= 66  & db$age < 69) {
  ## 66-68
  # Scale_Score
  db$kitchen_scale_score <- with ( db, ifelse (
    db$score >=  22, 18, ifelse (
      db$score >= 21, 17, ifelse (
        db$score >= 19 , 15, ifelse (
          db$score >= 17 , 14, ifelse (
            db$score >= 16, 13, ifelse (
              db$score >= 15, 12, ifelse (
                db$score >= 14, 11, ifelse (
                  db$score >= 12 , 10, ifelse (
                    db$score >= 11, 9, ifelse (
                      db$score >= 10, 8, ifelse (
                        db$score >= 9, 7, ifelse (
                          db$score >= 8, 6, ifelse (
                            db$score >= 7, 5, ifelse (
                              db$score <= 6, 2, NA )))))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      db$score >= 19 , "95-97" , ifelse (
        db$score >= 17 , "90-94" , ifelse (
          db$score >= 16, "82-89" , ifelse (
            db$score >= 15, "72-81" , ifelse (
              db$score >= 14, "60-71" , ifelse (
                db$score >= 12 , "41-59" , ifelse (
                  db$score >= 11, "29-40" , ifelse (
                    db$score >= 10, "19-28" , ifelse (
                      db$score >= 9, "11-18" , ifelse (
                        db$score >= 8, "6-10" , ifelse (
                          db$score >= 7, "3-5" , ifelse (
                            db$score <= 6, "<1" , NA )))))))))))))))
}
  
  ##############################TABLE 8##################################
  if(db$age >= 69  & db$age < 72) {
    ## 69-71
    # Scale_Score
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 21, 17, ifelse (
          db$score >= 19 , 15, ifelse (
            db$score >= 17 , 14, ifelse (
              db$score >= 16, 13, ifelse (
                db$score >= 15, 12, ifelse (
                  db$score >= 13 , 11, ifelse (
                    db$score >= 12, 10, ifelse (
                      db$score >= 11, 9, ifelse (
                        db$score >= 10, 8, ifelse (
                          db$score >= 9, 7, ifelse (
                            db$score >= 8, 6, ifelse (
                              db$score >= 7, 5, ifelse (
                                db$score <= 6, 2, NA )))))))))))))))

# percentile score

db$kitchen_percentil_range <- with(db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      db$score >= 19 , "95-97" , ifelse (
        db$score >= 17 , "90-94" , ifelse (
          db$score >= 16, "82-89" , ifelse (
            db$score >= 15, "72-81" , ifelse (
              db$score >= 13 , "60-71" , ifelse (
                db$score >= 12, "41-59" , ifelse (
                  db$score >= 11, "29-40" , ifelse (
                    db$score >= 10, "19-28" , ifelse (
                      db$score >= 9, "11-18" , ifelse (
                        db$score >= 8, "6-10" , ifelse (
                          db$score >= 7, "3-5" , ifelse (
                            db$score <= 6, "<1" , NA )))))))))))))))
  }
  
  ##############################TABLE 9##################################
  if(db$age >= 72  & db$age < 75) {
    ## 72-74
    # Scale_Score
    
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 21, 17, ifelse (
          db$score >= 19 , 15, ifelse (
            db$score >= 17 , 14, ifelse (
              db$score >= 15 , 13, ifelse (
                db$score >= 14, 12, ifelse (
                  db$score >= 13, 11, ifelse (
                    db$score >= 11 , 10, ifelse (
                      db$score >= 10, 8, ifelse (
                        db$score >= 9, 7, ifelse (
                          db$score >= 7, 6, ifelse (
                            db$score >= 6, 4, ifelse (
                              db$score <= 5, 2, NA ))))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      db$score >= 19 , "95-97" , ifelse (
        db$score >= 17 , "90-94" , ifelse (
          db$score >= 15 , "82-89" , ifelse (
            db$score >= 14, "72-81" , ifelse (
              db$score >= 13, "60-71" , ifelse (
                db$score >= 11 , "41-59" , ifelse (
                  db$score >= 10, "19-28" , ifelse (
                    db$score >= 9, "11-18" , ifelse (
                      db$score >= 7, "6-10" , ifelse (
                        db$score >= 6, "2" , ifelse (
                          db$score <= 5, "<1" , NA ))))))))))))))
  }
  
  ##############################TABLE 10##################################
  if(db$age >= 75  & db$age < 78) {
    ## 75-77
    # Scale_Score
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  22, 18, ifelse (
        db$score >= 21, 17, ifelse (
          db$score >= 20, 16, ifelse (
            db$score >= 18 , 15, ifelse (
              db$score >= 16 , 14, ifelse (
                db$score >= 14 , 13, ifelse (
                  db$score >= 13, 12, ifelse (
                    db$score >= 12, 11, ifelse (
                      db$score >= 11, 10, ifelse (
                        db$score >= 10, 9, ifelse (
                          db$score >= 9, 8, ifelse (
                            db$score >= 8, 7, ifelse (
                              db$score >= 7, 6, ifelse (
                                db$score >= 6, 5, ifelse (
                                  db$score <= 5, 2, NA ))))))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  22, "> 99" , ifelse (
    db$score >= 21, "99" , ifelse (
      db$score >= 20, "98" , ifelse (
        db$score >= 18 , "95-97" , ifelse (
          db$score >= 16 , "90-94" , ifelse (
            db$score >= 14 , "82-89" , ifelse (
              db$score >= 13, "72-81" , ifelse (
                db$score >= 12, "60-71" , ifelse (
                  db$score >= 11, "41-59" , ifelse (
                    db$score >= 10, "29-40" , ifelse (
                      db$score >= 9, "19-28" , ifelse (
                        db$score >= 8, "11-18" , ifelse (
                          db$score >= 7, "6-10" , ifelse (
                            db$score >= 6, "3-5" , ifelse (
                              db$score <= 5, "<1" , NA ))))))))))))))))

  }
  
  ##############################TABLE 11##################################
  if(db$age >= 78  & db$age < 81) {
    ## 78-80
    # Scale_Score
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  19, 18, ifelse (
        db$score >= 17 , 16, ifelse (
          db$score >= 16, 15, ifelse (
            db$score >= 14 , 14, ifelse (
              db$score >= 13, 13, ifelse (
                db$score >= 12, 12, ifelse (
                  db$score >= 11, 11, ifelse (
                    db$score >= 10, 10, ifelse (
                      db$score >= 9, 8, ifelse (
                        db$score >= 8, 7, ifelse (
                          db$score >= 7, 6, ifelse (
                            db$score >= 6, 5, ifelse (
                              db$score <= 5, 2, NA ))))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  19, "> 99" , ifelse (
    db$score >= 17 , "98" , ifelse (
      db$score >= 16, "95-97" , ifelse (
        db$score >= 14 , "90-94" , ifelse (
          db$score >= 13, "82-89" , ifelse (
            db$score >= 12, "72-81" , ifelse (
              db$score >= 11, "60-71" , ifelse (
                db$score >= 10, "41-59" , ifelse (
                  db$score >= 9, "19-28" , ifelse (
                    db$score >= 8, "11-18" , ifelse (
                      db$score >= 7, "6-10" , ifelse (
                        db$score >= 6, "3-5" , ifelse (
                          db$score <= 5, "<1" , NA ))))))))))))))
  }
  
  #############################TABLE 12###################################
  if(db$age >= 81  & db$age < 91) {
    ## 81-90
    # Scale_Score
    db$kitchen_scale_score <- with ( db, ifelse (
      db$score >=  15, 18, ifelse (
        db$score >= 14, 16, ifelse (
          db$score >= 13, 15, ifelse (
            db$score >= 12, 13, ifelse (
              db$score >= 11, 11, ifelse (
                db$score >= 10, 10, ifelse (
                  db$score >= 9, 9, ifelse (
                    db$score >= 8, 8, ifelse (
                      db$score >= 7, 6, ifelse (
                        db$score >= 6, 5, ifelse (
                          db$score >= 5, 4, ifelse (
                            db$score <= 4, 2, NA )))))))))))))

# percentile score

db$kitchen_percentil_range <- with (db, ifelse (
  db$score >=  15, "> 99" , ifelse (
    db$score >= 14, "98" , ifelse (
      db$score >= 13, "95-97" , ifelse (
        db$score >= 12, "82-89" , ifelse (
          db$score >= 11, "60-71" , ifelse (
            db$score >= 10, "41-59" , ifelse (
              db$score >= 9, "29-40" , ifelse (
                db$score >= 8, "19-28" , ifelse (
                  db$score >= 7, "6-10" , ifelse (
                    db$score >= 6, "3-5" , ifelse (
                      db$score >= 5, "2" , ifelse (
                        db$score <= 4, "<1" , NA )))))))))))))
  }
  
  # Educational level adjust ##C NO SE PUEDE AJUSTAR PORQUE NO ESTÁ EN ARTÍCULO NEURONORMAS
  #db$education_years_adj <- with(db, ifelse(
    #db$education_years >= 0  & db$education_years <= 3, db$kitchen_scale_score + 1, ifelse(
     # db$education_years >= 4  & db$education_years <= 8, db$kitchen_scale_score, ifelse(
       # db$education_years >= 9  & db$education_years <= 12, db$kitchen_scale_score - 1, ifelse(
          #db$education_years >= 13  & db$education_years <= 17, db$kitchen_scale_score - 2, ifelse(
           # db$education_years >= 18  & db$education_years <= 20, db$kitchen_scale_score- 3, ifelse(
            #)))))))

  
  # NSSae
  #db$NSSae_kitchen <- db$kitchen_scale_score - (-0.21832*(db$education_years_adj-12)) ####CAMBIAR
  
  return(db)
}

