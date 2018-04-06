library(haven)

ZZHR62FL <- read_dta("C:/Users/un144/Downloads/ZZHR62DT (1)/ZZHR62FL.DTA")


HR_sample = sample_n(ZZHR62FL,100,replace=FALSE)
## HR_top = top_n(ZZHR62FL,50) # this did not work, maybe too many columns
HR_top = ZZHR62FL[1:50,]

for (i in 1:7654) {attributes(HR_top[[i]]) = attributes(ZZHR62FL[[i]])}

rm(ZZHR62FL)

save(HR_top, file="HR_top.RData")
save(HR_sample, file="HR_sample.RData")


# find vars you want based on labels
HR_vars=matrix(NA,7654,2)
for (i in 1:7654) {HR_vars[i,2] = attributes(HR_top[[i]])$label}
HR_vars[,1] = names(HR_top)[1:7654]
HR_vars[str_detect(HR_vars[,2], "age"),]


save(HR_vars, file="HR_vars.RData")