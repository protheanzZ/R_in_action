library(vcd)

mytable <- with(Arthritis, table(Improved))
mytable


prop.table(mytable)

tab1 <- table(Arthritis$Treatment, Arthritis$Improved)
tab1

tab2 <- xtabs(~ Treatment + Improved, data=Arthritis)
tab2

head(Arthritis)

margin.table(tab2, 1)
margin.table(tab2, 2)

margin.table(tab2)

addmargins(tab2)

library(gmodels)

CrossTable(Arthritis$Treatment, Arthritis$Improved)
