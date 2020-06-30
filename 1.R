library(readxl)
library(dplyr)

payroll <- read_excel ("P01_LA zipcode payroll.xlsx", sheet = 13)

total=payroll[,c(1,5)]
total <- total[grep("Total", total$"Zip Code"),]
colnames(total) <- c("Zip Code", "Total")
total[]=lapply(total, gsub, pattern="\\ Total*", replacement="")
colnames(total) <- c("Zip Code", "Total")

payroll[,c(1,5:6)]=sapply(payroll[c(1,5:6)], as.numeric)
payroll[is.na(payroll)] <-0
newpayroll <- payroll[,c("Zip Code", "Industry", "Employment")]
newpayroll[]=lapply(newpayroll, gsub, pattern=",", replacement="")
newpayroll[]=lapply(newpayroll, gsub, pattern="&", replacement="")

info = subset(newpayroll, newpayroll$"Industry" == "Information")
info <- info [c(1,3)]
colnames(info) <- c("Zip Code", "Information")

prof = subset(newpayroll, newpayroll$"Industry" == "Professional Scientific  Technical Skills")
prof <- prof[c(1,3)]
colnames(prof) <- c("Zip Code", "Professional")

final=merge(info, prof, by="Zip Code", all=TRUE)

final2=merge(total, final, by="Zip Code", all=TRUE)
final2[,c(2:4)]=sapply(final2[c(2:4)], as.numeric)   
# if want to set variable as "0", use: final2[is.na(final2)] <-0
colnames(final2) <- c("Zip Code", "Total", "Information", "Professional")

sum = transform(rowSums(sapply(final2[, c(3:4)], as.numeric)))
colnames(sum) <- c("Sum")

final2$Percentage = sum$Sum/final2$Total
colnames(final2) <- c("Zip Code", "Total", "Information", "Professional Scientific Technical Skills", "Percentage")

write.csv (final2, file = "Project1.csv")
