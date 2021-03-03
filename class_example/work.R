head(msleep)
data = msleep
library(Stat2Data)
data("Backpack")
str(Backpack)
summary(Backpack)
df$x=as.factor(df$x)
Backpack$Year <- relevel(Backpack$Year)


