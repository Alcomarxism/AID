df <- read.csv2("var_15.csv")


#3
names(df)[names(df) == "Average.income"] <- "Average income"
names(df)[names(df) == "Work.experience"] <- "Work experience"
names(df)[names(df) == "Professional.specialization"] <- "Professional specialization"
names(df)[names(df) == "Average.number.of.pages"] <- "Average number of pages"
names(df)[names(df) == "Activity.level..score."] <- "Activity level (score)"
names(df)[names(df) == "Activity.level..level."] <- "Activity level (level)"

#4
df$"Group" <- as.factor(df$"Group")
df$"Gender" <- as.factor(df$"Gender")
df$"Age" <- as.integer(df$"Age")
df$"Work experience" <- as.numeric(df$"Work experience")
df$"Average income" <- as.numeric(df$"Average income")
df$"Professional specialization" <- as.factor(df$"Professional specialization")
df$"Average number of pages" <- as.integer(df$"Average number of pages")
df$"Activity level (score)" <- as.integer(df$"Activity level (score)")
df$"Activity level (level)" <- as.factor(df$"Activity level (level)")

#5
print(summary(df))


#6
par(mfrow = c(2, 2))
#6.1
plot(df$"Work experience", df$"Average income",main = "Диаграмма рассеяния", xlab = "Work experience", ylab = "Average income", pch = 11)

#6.2
x <- c(summary(df$"Professional specialization"))
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Professional specialization", col=c("red", "blue","green","yellow"),clockwise=TRUE)
legend("topright", c("1","2","3","4"), fill =c("red", "blue","green","yellow"))
#6.3