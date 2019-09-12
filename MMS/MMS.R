#
# Mini Mental
#


mini_mental_state <- function(score, age, education_years) {
  
  MMS <- data.frame(score = score, age = age, education_years = education_years)
  MMS_new <- data.frame()
  
  
  for (i in 1:nrow(MMS)) {
    res <- scale_score_MMS(score = MMS[i, "score"],
                           age = MMS[i, "age"], 
                         education_years = MMS[i, "education_years"])
    MMS_new <- rbind(MMS_new, res)
  }
  
  return(MMS_new[c("MMS","corrected_MMS")])
  
}


scale_score_MMS <- function(score, age, education_years) {
  
  db <- data.frame(score = score, age = age, education_years = education_years)
  
  if(db$age <= 50) {
    db$MMS <- with(db, ifelse(
      is.na(db$score), NA, ifelse (
      db$education_years <= 8, 0, ifelse(
        db$education_years < 18, -1, ifelse(
          db$education_years > 17, -2, NA
            )))))
  }
        
  
  if(db$age >50 | db$age < 76) {
    db$MMS <- with(db, ifelse(
      is.na(db$score), NA, ifelse (
      db$education_years <= 8, 1, ifelse(
        db$education_years < 18, 0, ifelse(
          db$education_years > 17, -1, NA
        )))))
  }
  
  if(db$age > 75) {
    db$MMS <- with(db, ifelse(
      is.na(db$score), NA, ifelse (
      db$education_years <= 8, 2, ifelse(
        db$education_years < 18, 1, ifelse(
          db$education_years > 17, 0, NA
        )))))
  }
  
  
  db$corrected_MMS <- with(db, ifelse(
    is.na(db$MMS), NA, ifelse(
      !is.na(db$MMS), db$score + db$MMS) ))
  
  
  return(db)
        
}
