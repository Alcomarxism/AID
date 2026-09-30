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
#6.1
dev.new()
plot(df$"Work experience", df$"Average income",main = "Диаграмма рассеяния", xlab = "Стаж работы", ylab = "Средний доход", pch = 11)

#6.2
dev.new()
x <- c(summary(df$"Professional specialization"))
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Профессиональная специализация", col=c("red", "blue","green","yellow"),clockwise=TRUE)
legend("topright", c("Не использую","Крайне редко","Ежедневно,","Постоянно"), fill =c("red", "blue","green","yellow"))

#6.3
dev.new()
par(mfrow = c(2, 2))
x <- c(df$"Activity level (level)"[(df$"Gender"=="1")&(df$"Group"=="1")])
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Мужчины первой группы", col=c("red", "blue","green","yellow"),clockwise=TRUE)
legend("topright", c("Не использую","Крайне редко","Ежедневно,","Постоянно"), fill =c("red", "blue","green","yellow"))


#6.4
dev.new()
par(mfrow = c(2, 2))
barplot(table(df$"Activity level (score)"[(df$"Gender"=="1")&(df$"Group"=="1")]),col=c("red"),main="Мужчины первой группы",xlab="Степень активности", ylab="Количество наблюдений")
barplot(table(df$"Activity level (score)"[(df$"Gender"=="1")&(df$"Group"=="2")]),col=c("blue"),main="Мужчины второй группы",xlab="Степень активности", ylab="Количество наблюдений")
barplot(table(df$"Activity level (score)"[(df$"Gender"=="2")&(df$"Group"=="1")]),col=c("green"),main="Женщины первой группы",xlab="Степень активности", ylab="Количество наблюдений")
barplot(table(df$"Activity level (score)"[(df$"Gender"=="2")&(df$"Group"=="2")]),col=c("yellow"),main="Женщины второй группы",xlab="Степень активности", ylab="Количество наблюдений")
