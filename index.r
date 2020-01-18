onch = read.csv("dataset.csv")
onch.dx.groups = read.csv("dx_conversion.csv")
onch.mean = mean(onch$CASES)
onch.all_cases = sum(onch$CASES)
onch.male_cases = onch$CASES[onch$SEX == "Male"]
onch.male_n = onch$N[onch$SEX == "Male"]
onch.male_prev = onch.male_cases/ onch.male_n
onch.male_prev_avg = mean(onch.male_prev,na.rm = T)
onch.female_cases = onch$CASES[onch$SEX == "Female"]
onch.female_n = onch$N[onch$SEX == "Female"]
onch.female_prev = onch.female_cases/ onch.female_n
onch.female_prev_avg = mean(onch.female_prev,na.rm = T)
onch.both_cases = onch$CASES[onch$SEX == "Both"]
onch.ss = onch$CASES[onch$DX_GROUP == "ss"]
onch.ss_total = sum(onch.ss)
onch.types_of_dx_cases = c() 
library(dplyr)
newdata = filter(onch,onch$LOC_GROUP == 1217)
newdata.prev =  sum(newdata$CASES)/ sum(newdata$N) *1000
