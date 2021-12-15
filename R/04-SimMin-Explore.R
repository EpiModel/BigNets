
## 04 Sim Min Exploratory Analysis

library("EpiModelHIV")
library("EpiModelHPC")

?process_simfiles

d <- merge_simfiles(simno = "3000", indir = "data/output/")

d
plot(d, y = "num", ylim = c(50000, 150000))
plot(d, y = "incid")
plot(d, y = "ir100.sti")
plot(d, y = "new.aids.tot")

df <- as.data.frame(d)
head(df)
