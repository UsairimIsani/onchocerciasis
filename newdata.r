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
newdata.mean_cases = mean(newdata$CASES)
newdata.std_dev = sd(newdata$CASES)
newdata.male = filter(newdata, newdata$SEX == "Male") # Males
newdata.male_cases = sum(newdata.male$CASES) # All Male Caes
newdata.female = filter(newdata, newdata$SEX == "Female")
newdata.female_cases = sum(newdata.female$CASES)

newdata.male_c = sum(newdata.male$N) # All Male Caes
newdata.male_prev = newdata.male_cases / newdata.male_c * 1000
newdata.female_c = sum(newdata.female$N)
newdata.female_prev = newdata.female_cases / newdata.female_c * 1000

newdata.ss_cases = filter(newdata,newdata$DX_GROUP == "ss")
newdata.ss_cases_c = sum(newdata.ss_cases$CASES)

newdata.skin_cases = filter(newdata,newdata$DX_GROUP == "skin")
newdata.skin_cases_c = sum(newdata.skin_cases$CASES)

newdata.eye_cases = filter(newdata,newdata$DX_GROUP == "eye")
newdata.eye_cases_c = sum(newdata.eye_cases$CASES)

newdata.nod_cases = filter(newdata,newdata$DX_GROUP == "nod")
newdata.nod_cases_c = sum(newdata.nod_cases$CASES)

newdata.male_under_nine = filter(newdata.male,newdata.male$AGE_END <= 9)
newdata.male_under_nine_cases = sum(newdata.male_under_nine$CASES)
newdata.male_bt_10_and_19 = filter(newdata.male,newdata.male$AGE_START >= 10, newdata.male$AGE_END == 19)
newdata.male_bt_10_and_19_cases=sum(newdata.male_bt_10_and_19$CASES)
newdata.male_bt_20_and_29 = filter(newdata.male,newdata.male$AGE_START >= 20 ,newdata.male$AGE_END <= 29)
newdata.male_bt_20_and_29_cases=sum(newdata.male_bt_20_and_29$CASES)
newdata.male_bt_30_and_39 = filter(newdata.male,newdata.male$AGE_START==30 ,newdata.male$AGE_END == 39)
newdata.male_bt_30_and_39_cases=sum(newdata.male_bt_30_and_39$CASES)
newdata.male_bt_40_and_49 = filter(newdata.male,newdata.male$AGE_START==40 , newdata.male$AGE_END == 49)
newdata.male_bt_40_and_49_cases=sum(newdata.male_bt_40_and_49$CASES)
newdata.male_greater_50 = filter(newdata.male,newdata.male$AGE_START>=50)
newdata.male_greater_50_cases=sum(newdata.male_greater_50$CASES)

newdata.female_under_nine = filter(newdata.female,newdata.female$AGE_END <= 9)
newdata.female_bt_10_and_19 = filter(newdata.female,newdata.female$AGE_START==10 , newdata.female$AGE_END == 19)
newdata.female_bt_20_and_29 = filter(newdata.female,newdata.female$AGE_START==20 , newdata.female$AGE_END == 29)
newdata.female_bt_30_and_39 = filter(newdata.female,newdata.female$AGE_START==30, newdata.female$AGE_END == 39)
newdata.female_bt_40_and_49 = filter(newdata.female,newdata.female$AGE_START==40 , newdata.female$AGE_END == 49)
newdata.female_greater_50 = filter(newdata.female,newdata.female$AGE_START >= 50)
newdata.female_under_nine_cases=sum(newdata.female_under_nine$CASES)
newdata.female_bt_10_and_19_cases=sum(newdata.female_bt_10_and_19$CASES)
newdata.female_bt_20_and_29_cases=sum(newdata.female_bt_20_and_29$CASES)
newdata.female_bt_30_and_39_cases=sum(newdata.female_bt_30_and_39$CASES)
newdata.female_bt_40_and_49_cases=sum(newdata.female_bt_40_and_49$CASES)
newdata.female_greater_50_cases=sum(newdata.female_greater_50$CASES)

dataplot_male = c(newdata.male_under_nine_cases, newdata.male_bt_10_and_19_cases,newdata.male_bt_20_and_29_cases)
dataplot_female = c(newdata.female_under_nine_cases, newdata.female_bt_10_and_19_cases,newdata.female_bt_20_and_29_cases)
age_grp = c("under 9","between 10 and 19","between 20 and 29","between 30 and 39","between 40 and 49","greater than 50")
#plot(x= age_grp,y= dataplot_male,xlab= "Age Groups",ylab="Values",xlim = )
#edit(newdata)
no_cases_female=c(newdata.female_under_nine_cases,newdata.female_bt_10_and_19_cases,newdata.female_bt_20_and_29_cases,newdata.female_bt_30_and_39_cases,newdata.female_bt_40_and_49_cases,newdata.female_greater_50_cases)
no_cases_male=c(newdata.male_under_nine_cases,newdata.male_bt_10_and_19_cases,newdata.male_bt_20_and_29_cases,newdata.male_bt_30_and_39_cases,newdata.male_bt_40_and_49_cases,newdata.male_greater_50_cases)
#barplot(as.matrix(no_cases_male),ylab='cases',beside=T,col=rainbow(2),ylim = c(0,200),space = 0.2)
#barplot(as.matrix(no_cases_male),ylab='cases',beside=T,col=rainbow(2),ylim = c(0,200),space = 0.2)
#legend('topleft',c('male','female'),cex=0.6,bty='n',fill=rainbow(2))
barplot(no_cases_female,names.arg = age_grp,main = "Cases in Female Age Group")
barplot(no_cases_male,names.arg = age_grp,main = "Cases in Male Age Group")
pie(c(newdata.ss_cases_c,newdata.skin_cases_c,newdata.eye_cases_c,newdata.nod_cases_c),labels = c("Skin Snip","Skin Lesions","Vision Impairement","Nodule"),main = "Diagnostic Groups")
summary(newdata)
