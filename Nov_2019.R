## Nov 2019

library(readxl)

bd_nov_2019 <- read_xlsx("Cognitivo/BD_COGNITIVA_2.xlsx")
names(bd_nov_2019)

# PRE
prueba <- neuronorma(score_test = list(Q_COG_TMT_A_PRE = "Trail making test a", 
                                      Q_COG_TMT_B_PRE = "Trail making test b",
                                      Q_COG_WAIS_FORWARD_PRE = "Digit span forward",
                                      Q_COG_WAIS_BACKWARD_PRE = "Digit span backward",
                                      Q_COG_STROOP_W_DP_PRE = "Stroop color-word interference test (word)",
                                      Q_COG_STROOP_C_DP_PRE = "Stroop color-word interference test (color)",
                                      Q_COG_STROOP_WC_DP_PRE = "Stroop color-word interference test (interference)",
                                      Q_COG_COWAT_ANIMAL_PRE = "COWAT_animal",
                                      Q_COG_COWAT_FRUIT_PRE = "COWAT_fruit",
                                      Q_COG_COWAT_KITCHEN_PRE = "COWAT_kitchen",
                                      Q_COG_COWAT_M_PRE = "COWAT_m",
                                      Q_COG_COWAT_P_PRE = "COWAT_p",
                                      Q_COG_COWAT_R_PRE = "COWAT_r",
                                      Q_COG_MMSE_OVERALL_PRE = "mini mental",
                                      age = "AGE_PRE",
                                      education_years = "EDUCATIONAL_YEARS",
                                      db = bd))



## WORD y COLOR
db_prueba <- bd_nov_2019[1:10, c("Q_COG_STROOP_W_DP_PRE", "Q_COG_STROOP_C_DP_PRE")]

prueba_neuronorma <- neuronorma(score_test = list(
                                       Q_COG_STROOP_W_DP_PRE = "Stroop color-word interference test (word)",
                                       Q_COG_STROOP_C_DP_PRE = "Stroop color-word interference test (color)"),
                                       age = "AGE_PRE",
                                       education_years = "EDUCATIONAL_YEARS",
                                       db = db_prueba)


