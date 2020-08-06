
form_contrasts <- function(dataset1, dataset2) {
  
  numbers <- table(dataset1$status_2_1)  #dataset 1 is used to determine weights of contrasts,
  #dataset2 is dataset we want to apply contrasting to
  
  dataset2$c1 <- ifelse(dataset2$status_2_1 == "NA_1", -2, 
                        ifelse(dataset2$status_2_1 == "NA_0", -2, 1))
  
  dataset2$c2 <- ifelse(dataset2$status_2_1 == "NA_1", (1 / numbers[6]), 
                        ifelse(dataset2$status_2_1 == "NA_0", -(1 / numbers[5]) , 0))
  
  dataset2$c3 <- ifelse(dataset2$status_2_1 == "0_1", (1 / (numbers[2] + numbers[4])), 
                        ifelse(dataset2$status_2_1 == "1_1", (1 / (numbers[2] + numbers[4])), 
                               ifelse(dataset2$status_2_1 == "1_0", -(1 / (numbers[1] + numbers[3])), 
                                      ifelse(dataset2$status_2_1 == "0_0", -(1 / (numbers[1] + numbers[3])), 0))))
  
  dataset2$c4 <- ifelse(dataset2$status_2_1 == "0_1", (1 / numbers[2]) , ifelse(dataset2$status_2_1 == "1_1", -(1 / numbers[4]), 0))
  dataset2$c5 <- ifelse(dataset2$status_2_1 == "1_0", (1 / numbers[3]), ifelse(dataset2$status_2_1 == "0_0", -(1 / numbers[1]), 0))
  
  dataset2
}
